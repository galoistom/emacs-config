;; 初始化 EMACS 内置的包管理系统
(setq url-proxy-services
      '(("http"  . "127.0.0.1:7890")
        ("https" . "127.0.0.1:7890")))
(set-fontset-font (frame-parameter nil 'font) 
                  'unicode 
                  (font-spec :family "Hack Nerd Font")) 
(set-face-attribute 'default nil 
  :family "Cascadia Code"   ; 替换为你想要的主字体
  :height 140                ; 设置字体大小，120 等于 12pt
  :weight 'normal)
;;(set-fontset-font "fontset-default" '(#xe000 . #xf8ff) "Hack Nerd Font")
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode t)
(window-divider-mode t)
(electric-pair-mode 1)
(global-hl-line-mode t)
(setq display-time-format "%Y-%m-%d %H:%M")
(require 'package)

;; 添加 MELPA 仓库源
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))
;; 加载所有已安装的包并同步仓库列表
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2d74de1cc32d00b20b347f2d0037b945a4158004f99877630afc034a674e3ab7"
     "4dcf06273c9f5f0e01cea95e5f71c3b6ee506f192d75ffda639d737964e2e14e"
     default))
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.5 :html-foreground
		 "Black" :html-background "Transparent" :html-scale
		 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")
		 :use-image-magick t))
 '(org-format-latex-signal-error nil)
 '(org-latex-packages-alist '(("" "tikz-cd" t nil) ("" "tikz" t nil)))
 '(org-preview-latex-default-process 'dvisvgm)
 '(package-selected-packages
   '(catppuccin-theme company-box cond-let dashboard doom-modeline
		      dracula-theme esh-autosuggest esh-help
		      eshell-autojump eshell-did-you-mean
		      eshell-git-prompt eshell-syntax-highlighting
		      evil-collection evil-leader lsp-mode mason
		      pdf-tools reformatter rust-mode slime texfrag
		      typescript-mode vertico with-editor xterm-color)))
;;(load-theme 'catppuccin :no-confirm)
(load-theme 'dracula :no-confirm)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq custom-config-dir (expand-file-name "config/" user-emacs-directory))
(load (concat custom-config-dir "evil.el"))
(load (concat custom-config-dir "markdown.el"))
(load (concat custom-config-dir "lsp.el"))
(load (concat custom-config-dir "org.el"))
;;(load (concat custom-config-dir "lsp.el"))

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(set-frame-parameter (selected-frame) 'background-mode 'dark) 
;;(set-face-attribute 'default nil 
;;                    :background "#000000")
(setq xterm-query-color-support t)
(unless (display-graphic-p)
  (setq-default frame-background-mode nil)
  (setq term-setup-hook (lambda ()
                          (set-terminal-parameter nil 'frame-background-mode 'dark))))

(require 'dashboard)
(dashboard-setup-startup-hook)
;;(dashboard-mode 1)
;;
;;;; 可选：自定义标题 (例如欢迎信息)
;;
(setq dashboard-banner-logo-title "EVIL")
;;;; 可选：自定义显示哪些项目（例如最近文件和项目）
(setq dashboard-items '((recents . 10)
			(bookmarks . 5))) ; 显示 5 个待办事项
;;(setq dashboard-center-content t)
(setq dashboard-vertically-center-content t)

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project) 
(setq doom-modeline-minor-modes nil)
  
)
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

