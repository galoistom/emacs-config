;;; -*- lexical-binding: t; -*-
;---- regular configuration ----
(add-to-list 'custom-theme-load-path
             (expand-file-name "dracula" user-emacs-directory))

(load-theme 'dracula t)
(setq url-proxy-services
      '(("http"  . "127.0.0.1:7890")
        ("https" . "127.0.0.1:7890")))
(require 'socks)

(setq socks-server '("socks" "127.0.0.1" 7891 5))
(defun my/setup-fonts (&optional frame)
  "Set up fonts for FRAME."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (set-face-attribute 'default nil
                          :family "Cascadia Code"
                          :height 140
                          :weight 'normal)

      (set-fontset-font
       "fontset-default"
       'unicode
       (font-spec :family "Hack Nerd Font")
       nil
       'append))))

(add-hook 'after-init-hook #'my/setup-fonts)

(add-hook 'after-make-frame-functions #'my/setup-fonts)
(global-display-line-numbers-mode t)
(set-frame-parameter (selected-frame) 'background-mode 'dark)

(setq-default tab-width 8)
(setq-default indent-tabs-mode nil)
(setq standard-indent 8)
(setq inhibit-startup-screen t)
(setq select-enable-clipboard t)
(setq select-enable-primary t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(window-divider-mode t)
(electric-pair-mode 1)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-margin 9)
(global-auto-revert-mode 1)
(ido-mode t)
(setq dired-listing-switches "-alhn")
(setq gc-cons-threshold (* 16 1024 1024))
(setq read-process-output-max (* 3 1024 1024))
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")
(defun my/create-non-existent-directories ()
  "Create diction when not exists."
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

(modify-syntax-entry ?< "." (standard-syntax-table))
(modify-syntax-entry ?> "." (standard-syntax-table))
(let ((path (shell-command-to-string "source ~/.zshrc && echo $PATH")))
  (setenv "PATH" path)
  (setq exec-path (split-string path ":" t)))

(set-char-table-parent (standard-syntax-table) nil)
;---- basic packages -----
(require 'package)
(use-package emacs
  :init
  (setq tab-always-indent 'complete))

(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar custom-config-dir)
(setq custom-config-dir (expand-file-name "config/" user-emacs-directory))
(load (concat custom-config-dir "scheme-config"))
(load (concat custom-config-dir "org"))
(load (concat custom-config-dir "markdown"))
(load (concat custom-config-dir "lsp"))
(load (concat custom-config-dir "my-fill"))
(load (concat custom-config-dir "eshell"))
(load (concat custom-config-dir "basic"))
(load (concat custom-config-dir "modeline"))
(load (concat custom-config-dir "emskin"))
(load (concat custom-config-dir "ai"))
(load (concat custom-config-dir "mail"))
(load (concat custom-config-dir "modeline"))
(load (concat custom-config-dir "keymap"))
(setq custom-file (expand-file-name "custom" user-emacs-directory))
(load custom-file 'noerror)
(setq auto-mode-alist
      (rassq-delete-all 'scheme-mode auto-mode-alist))
