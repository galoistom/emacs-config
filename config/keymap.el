;;; -*- lexical-binding: t; -*-
;;; Code:
(add-to-list 'load-path "/home/galoistom/emskin/elisp")
(require 'dired)
(require 'org)
(require 'eglot)
(require 'multiple-cursors)
(require 'xdg-launcher)
(require 'notmuch)
(require 'fzf)
(require 'ghostel)
(require 'hideshow)
(require 'consult)

(defmacro my-lambda (&rest body)
  "A macro for simpler lambda with BODY."
  `(lambda () (interactive) ,@body))

(declare-function my-fill-function "my-fill")
(declare-function my-latex-math-auto-fill-mode  "my-fill")

(defun qutebrowser (url)
  "Start qutebrowser with URL."
  (interactive "sinput url: ")
  (start-process-shell-command "browser" nil (format "qutebrowser %s" url)))

(defun my/cheatsheet (name)
  "Search for cheatsheet.sh with NAME."
  (interactive "ssearch cheatsheet: ")
  (compile (format "curl cheat.sh/%s" name)))

(defun my/capital-forward ()
  "Capitalize-Word before cursor."
  (interactive)
  (backward-word)
  (capitalize-word 1))

(defun my/fzf-copy-home-path ()
  "Use fzf to find file and copy their path."
  (interactive)
  (fzf-with-command
   "find /home/galoistom -type f -not -path '*/.*'"
   (lambda (file)
     (when file
       (kill-new file)
       (message "copied: %s" file)))))

(define-minor-mode my-cj-mode
  "Force C-j to be C-x map."
  :global t
  :type 'boolean
  :group 'my-config
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-j")     ctl-x-map)
	    (define-key map (kbd "C-c C-d") #'backward-kill-word)
	    (define-key map (kbd "C-c d")   #'kill-word)
	    (define-key map (kbd "M-e")     #'mark-word)
            map))

(my-cj-mode 1)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c C-e") 'wdired-change-to-wdired-mode))

(define-prefix-command 'my/w-prefix)
(global-set-key (kbd "C-c w") 'my/w-prefix)
(global-set-key (kbd "C-c w t") #'split-window-below)
(global-set-key (kbd "C-c w v") #'split-window-right)
(global-set-key (kbd "C-c w d") #'kill-buffer-and-window)
(global-set-key (kbd "C-c w x") #'delete-window)
(global-set-key (kbd "C-c w f") #'delete-other-windows)
(global-set-key (kbd "C-c w b") #'balance-windows)
(global-set-key (kbd "C-c w s") #'window-swap-states)
(global-set-key (kbd "C-c w m") #'maximize-window)
(global-set-key (kbd "C-c w ,") #'minimize-window)
(global-set-key (kbd "C-c w o") #'other-window)
(global-set-key (kbd "C-c w n") (my-lambda (switch-to-buffer "temp")))

(define-prefix-command 'my/hide-show)
(global-set-key (kbd "C-c C-v") 'my/hide-show)
(global-set-key (kbd "C-c h") #'hs-minor-mode)
(global-set-key (kbd "C-c C-v C-a") #'hs-show-all)
(global-set-key (kbd "C-c C-v C-t") #'hs-hide-all)
(global-set-key (kbd "C-c C-v C-c") #'hs-toggle-hiding)

(global-set-key (kbd "C-.")          #'duplicate-line)
(global-set-key (kbd "C-v")          #'my-fill-function)
(global-set-key (kbd "C-o")          #'flash-emacs-jump)
(global-set-key (kbd "s-s")          #'save-buffer)
(global-set-key (kbd "s-d")          #'backward-delete-char)
(global-set-key (kbd "C-<tab>")      #'other-window)
(global-set-key (kbd "M-\"")         #'shell-command)
(global-set-key (kbd "C-M-n")        #'mc/mark-next-like-this)
(global-set-key (kbd "C-M-p")        #'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-f")        #'up-list)
(global-set-key (kbd "C-M-s")        #'consult-line)

(global-set-key (kbd "C-x f")        #'fzf-find-file)
(global-set-key (kbd "C-x k")        #'goto-last-change)
(global-set-key (kbd "C-x c")        #'compile)
(global-set-key (kbd "C-x b")        #'consult-buffer)

(global-set-key (kbd "C-c b")        #'qutebrowser)
(global-set-key (kbd "C-c e")        #'eshell)
(global-set-key (kbd "C-c t")        #'ghostel)
(global-set-key (kbd "C-c z")        #'zap-to-char)
(global-set-key (kbd "C-c c")        #'my/capital-forward)
(global-set-key (kbd "C-c r")        #'rgrep)
(global-set-key (kbd "C-c D")        #'kill-whole-line)
(global-set-key (kbd "C-c i")        #'indent-region)
(global-set-key (kbd "C-c j")        #'mc/unmark-previous-like-this)
(global-set-key (kbd "C-c k")        #'mc/unmark-next-like-this)
(global-set-key (kbd "C-c l")        #'xdg-launcher-run-app)
(global-set-key (kbd "C-c F")        #'eglot-format-buffer)
(global-set-key (kbd "C-c R")        #'eglot-reconnect)
(global-set-key (kbd "C-c m")        #'my-latex-math-auto-fill-mode)
(global-set-key (kbd "C-c p")        #'math-preview-all)
(global-set-key (kbd "C-c P")        #'math-preview-clear-all)
(global-set-key (kbd "C-c S")        #'my/cheatsheet)
(global-set-key (kbd "C-c f")        #'fzf)
(global-set-key (kbd "C-c s")        #'consult-line)

(global-set-key (kbd "C-c C-l")      #'eglot)
(global-set-key (kbd "C-c C-h")      #'notmuch)
(global-set-key (kbd "C-x C-a")      #'replace-regexp)
(global-set-key (kbd "C-x C-l")      #'fzf-switch-buffer)
(global-set-key (kbd "C-x C-q")      #'kill-emacs)
