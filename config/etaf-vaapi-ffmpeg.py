#!/usr/bin/env python3
"""VAAPI ffmpeg wrapper for etaf-video-player.

ETAF invokes ffmpeg with a fixed argument contract.  This wrapper rewrites
the image/raw frame producer to decode and scale on the Intel VAAPI device,
then downloads the frames for the existing software MJPEG/raw output stage.
It falls back to the original software command when the hardware path
produces no output.  It leaves every other invocation,
including the screenshot command, untouched.

Set ETAF_VAAPI_DEVICE to override the detected /dev/dri/renderD* device and
ETAF_VAAPI_FFMPEG to point at a different ffmpeg binary.
"""

import ctypes
import glob
import os
import re
import signal
import subprocess
import sys


FFMPEG = os.environ.get("ETAF_VAAPI_FFMPEG", "ffmpeg")
DEVICE_ENV = "ETAF_VAAPI_DEVICE"
child = None


def find_device():
    """Return a VAAPI render device path, or None."""
    explicit = os.environ.get(DEVICE_ENV)
    if explicit:
        return explicit if os.path.exists(explicit) else None
    for pattern in ("/dev/dri/renderD*", "/dev/dri/card*"):
        matches = sorted(glob.glob(pattern))
        if matches:
            return matches[0]
    return None


def input_index(args):
    """Return the index of the single input -i, or None."""
    if args.count("-i") != 1:
        return None
    return args.index("-i")


def is_lavfi(args, index):
    """Return non-nil when the input before INDEX is a lavfi source."""
    for position in range(index - 1, max(-1, index - 16), -1):
        if args[position] == "-i":
            break
        if (args[position] == "-f" and position + 1 < len(args)
                and args[position + 1] == "lavfi"):
            return True
    return False


def output_details(args, index):
    """Return (FORMAT, CODEC, PIX_FMT, VIDEO_FILTER) after the input."""
    rest = args[index + 1:]
    fmt = codec = pix_fmt = video_filter = None
    position = 0
    while position + 1 < len(rest):
        option = rest[position]
        value = rest[position + 1]
        if option == "-f":
            fmt = value
        elif option in ("-c:v", "-vcodec"):
            codec = value
        elif option == "-pix_fmt":
            pix_fmt = value
        elif option == "-vf":
            video_filter = value
        position += 1
    return fmt, codec, pix_fmt, video_filter


def rewrite_scale(video_filter):
    """Rewrite the single scale filter, or return None when unsupported."""
    if not video_filter:
        return None
    parts = video_filter.split(",")
    for position, part in enumerate(parts):
        match = re.match(r"^scale=([0-9]+):([0-9]+)(?::flags=[^,]+)?$", part)
        if match:
            if len(parts) != 1:
                return None
            # Keep the frames on the GPU through decode and scale, then
            # download them for the software MJPEG/raw output stage.
            return "scale_vaapi=w=%s:h=%s:out_range=full,hwdownload,format=nv12" % (
                match.group(1), match.group(2))
    return None


def vaapi_arguments(args, index, fmt, codec, video_filter, device):
    """Return the VAAPI command arguments, or None when not applicable."""
    if fmt not in ("image2pipe", "rawvideo"):
        return None
    if fmt == "image2pipe" and codec != "mjpeg":
        return None
    rewritten = rewrite_scale(video_filter)
    if not rewritten:
        return None

    result = list(args)
    result[index:index] = [
        "-hwaccel", "vaapi",
        "-hwaccel_device", device,
        "-hwaccel_output_format", "vaapi",
    ]
    # The input moved to the right by the inserted options.
    index += 6
    position = index + 1
    while position + 1 < len(result):
        if result[position] == "-vf":
            result[position + 1] = rewritten
        position += 1
    return result


def pdeathsig():
    """Ask Linux to terminate the child when this wrapper dies."""
    try:
        libc = ctypes.CDLL(None)
        libc.prctl(1, signal.SIGTERM, 0, 0, 0)
    except Exception:
        pass


def stop_child(_signum, _frame):
    """Forward a termination signal to the running ffmpeg child."""
    if child is not None and child.poll() is None:
        try:
            child.terminate()
        except Exception:
            pass
    raise SystemExit(128 + _signum)


def run(command):
    """Run COMMAND, stream its stdout, and return (STATUS, BYTES)."""
    global child
    try:
        child = subprocess.Popen(
            command,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            preexec_fn=pdeathsig,
        )
    except OSError:
        return None, 0
    produced = 0
    try:
        while True:
            # read1 returns as soon as some bytes are available; plain read
            # would wait for a full buffer and add seconds of latency.
            chunk = child.stdout.read1(65536)
            if not chunk:
                break
            produced += len(chunk)
            sys.stdout.buffer.write(chunk)
            sys.stdout.buffer.flush()
    except BrokenPipeError:
        try:
            child.terminate()
        except Exception:
            pass
        return 0, produced
    finally:
        try:
            child.stdout.close()
        except Exception:
            pass
    return child.wait(), produced


def exec_software(args):
    """Replace this wrapper with the stock ffmpeg command."""
    try:
        os.execvp(FFMPEG, [FFMPEG] + args)
    except OSError as error:
        sys.stderr.write("etaf-vaapi: cannot exec %s: %s\n" % (FFMPEG, error))
        raise SystemExit(127)


def main():
    args = sys.argv[1:]
    device = find_device()
    index = input_index(args)
    if device is None or index is None or is_lavfi(args, index):
        exec_software(args)

    fmt, codec, _pix_fmt, video_filter = output_details(args, index)
    command = vaapi_arguments(args, index, fmt, codec, video_filter, device)
    if command is None:
        exec_software(args)

    signal.signal(signal.SIGTERM, stop_child)
    signal.signal(signal.SIGINT, stop_child)
    signal.signal(signal.SIGHUP, stop_child)

    status, produced = run([FFMPEG] + command)
    if status is not None and status != 0 and produced == 0:
        # The hardware path failed before sending a frame; restart with the
        # exact software command ETAF asked for.
        exec_software(args)
    raise SystemExit(status if status is not None else 127)


if __name__ == "__main__":
    main()
