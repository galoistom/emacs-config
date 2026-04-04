;;; -*- lexical-binding: t; -*-
;---- regular configuration ----
(add-to-list 'custom-theme-load-path
             (expand-file-name "dracula" user-emacs-directory))

(load-theme 'dracula t)
(setq url-proxy-services
      '(("http"  . "127.0.0.1:7890")
        ("https" . "127.0.0.1:7890")))
(defun my/setup-fonts (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      ;; 主字体
      (set-face-attribute 'default nil
                          :family "Cascadia Code"
                          :height 140
                          :weight 'normal)

      ;; Unicode / Nerd Font fallback
      (set-fontset-font
       "fontset-default"
       'unicode
       (font-spec :family "Hack Nerd Font")
       nil
       'append))))

;; GUI 启动
(add-hook 'after-init-hook #'my/setup-fonts)

;; daemon / emacsclient 新 frame
(add-hook 'after-make-frame-functions #'my/setup-fonts)
;(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(global-display-line-numbers-mode t)
(set-frame-parameter (selected-frame) 'background-mode 'dark)

(setq-default tab-width 8)
(setq standard-indent 8)
(setq inhibit-startup-screen t)
(setq select-enable-clipboard t)
(setq select-enable-primary t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode t)
(window-divider-mode t)
(electric-pair-mode 1)
(setq scroll-step 1)                ; 每次滚动 1 行
(setq scroll-conservatively 10000)
(setq scroll-margin 9)
(ido-mode t)
(defun my/create-non-existent-directories ()
  (let ((parent (file-name-directory buffer-file-name)))
    (when (and parent (not (file-exists-p parent)))
      (make-directory parent t))))

(add-hook 'find-file-not-found-functions
          #'my/create-non-existent-directories)
;; Wayland TUI clipboard bridge
(when (and (not (display-graphic-p))
           (executable-find "wl-copy")
           (executable-find "wl-paste"))
  (setq interprogram-cut-function
        (lambda (text)
          (let ((process-connection-type nil))
            (let ((proc (start-process "wl-copy" nil "wl-copy")))
              (process-send-string proc text)
              (process-send-eof proc)))))

  (setq interprogram-paste-function
        (lambda ()
          (when (executable-find "wl-paste")
            (string-trim-right
             (shell-command-to-string "wl-paste -n"))))))

(setq auto-save-default t)
(setq auto-save-timeout 5)
(setq auto-save-interval 50)
(setq display-time-format "%d %H:%M")
(setq shr-use-fonts nil)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")

;---- basic packages -----
(require 'package)

;; 添加 MELPA 仓库源
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))
;; 加载所有已安装的包并同步仓库列表
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(setq custom-config-dir (expand-file-name "config/" user-emacs-directory))
(load (concat custom-config-dir "org"))
(load (concat custom-config-dir "markdown"))
(load (concat custom-config-dir "lsp"))
(load (concat custom-config-dir "my-fill"))
(load (concat custom-config-dir "eshell"))
(load (concat custom-config-dir "scheme-config"))
(load (concat custom-config-dir "basic"))
(load (concat custom-config-dir "keymap"))
(setq custom-file (expand-file-name "custom" user-emacs-directory))
(load custom-file 'noerror)

