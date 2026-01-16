;--- regular configuration ---
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
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

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
(ido-mode t)
(ido-everywhere t)

(setq display-time-format "%Y-%m-%d %H:%M")

;--- basic packages ---
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
(load (concat custom-config-dir "evil.el"))
(load (concat custom-config-dir "markdown.el"))
(load (concat custom-config-dir "lsp.el"))
(load (concat custom-config-dir "org.el"))
;(load (concat custom-config-dir "my_fill.el"))
;(load (concat custom-config-dir "eshell.el"))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
(add-to-list 'custom-theme-load-path
             (expand-file-name "theme" user-emacs-directory))
(load-theme 'dracula t)

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

(set-frame-parameter (selected-frame) 'background-mode 'dark) 
(setq xterm-query-color-support t)
(unless (display-graphic-p)
  (setq-default frame-background-mode nil)
  (setq term-setup-hook (lambda ()
                          (set-terminal-parameter nil 'frame-background-mode 'dark))))

(use-package dashboard
  :ensure t
  :config
    (dashboard-setup-startup-hook)
    (setq dashboard-banner-logo-title "EVIL")
    (setq dashboard-items '((recents . 10)
			    (bookmarks . 5))) ; 显示 5 个待办事项
    (setq dashboard-vertically-center-content t))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project) 
  (setq doom-modeline-minor-modes nil))

(mason-ensure t)

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  ;; 设置默认以适合页面大小的方式显示 PDF
  (setq-default pdf-view-display-size 'fit-page)
  (push 'display-line-numbers-mode pdf-view-incompatible-modes)
  ;; 将 pdf-view-mode 设置为默认的 PDF 查看模式
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))

;; 激活 pdf-tools 的快捷键
(provide 'pdf-tools-config)

(use-package avy :ensure t)
(use-package eldoc-box :ensure t)

(setq lsp-meson-no-auto-downloads t)

(use-package dired-preview
  :ensure t
  :config
    (setq dired-preview-delay 0.05)
    ; set preview to right hand side
    (setq dired-preview-display-action-alist
	'((display-buffer-in-side-window)
	    (side . right)
	    (window-width . 0.5))))

(setq auto-save-default t)
(setq auto-save-timeout 5)
(setq auto-save-interval 50)

(add-hook 'pdf-view-mode-hook (lambda ()
				(display-line-numbers-mode -1)))

;; [rainbow-delimiters] Highlight brackets according to their depth
(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode conf-mode yaml-mode) . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5))

(set-face-attribute 'show-paren-match nil
                    :weight 'bold
                    :background "#44475a"
                    :foreground "#ffff00")

(global-set-key (kbd "C-S-h") 'dashboard-open)
