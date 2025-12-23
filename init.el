;; 初始化 EMACS 内置的包管理系统
;;(server-start)
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
;;(add-to-list 'exec-path "/home/user/.local/bin") ; 替换为你的 qutebrowser 路径
;;(add-to-list 'exec-path "/usr/local/bin")      ; 如果是 Homebrew 等安装的路径
(setq-default tab-width 8)
(setq standard-indent 8)
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode t)
(window-divider-mode t)
(electric-pair-mode 1)
;;(global-hl-line-mode t)
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
 '(dashboard-banner-ascii
   "\12███╗   ███╗██╗   ██╗██╗   ██╗██╗███╗   ███╗\12████╗ ████║╚██╗ ██╔╝██║   ██║██║████╗ ████║\12██╔████╔██║ ╚████╔╝ ██║   ██║██║██╔████╔██║\12██║╚██╔╝██║  ╚██╔╝  ╚██╗ ██╔╝██║██║╚██╔╝██║\12██║ ╚═╝ ██║   ██║    ╚████╔╝ ██║██║ ╚═╝ ██║\12╚═╝     ╚═╝   ╚═╝     ╚═══╝  ╚═╝╚═╝     ╚═╝\12")
 '(dashboard-center-content t)
 '(dashboard-footer-messages
   '("The one true editor, Evil!"
     "Who the hell uses Emacs anyway? Go Evil!"
     "Free as free speech, free as free Beer" "Happy coding!"
     "Vi Vi Vi, the editor of the best"
     "Welcome to the church of Evil"
     "While any text editor can save your files, only Evil can save your soul"
     "I showed you my source code, pls respond"))
 '(dashboard-path-style 'truncate-end)
 '(dashboard-startup-banner 'ascii)
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(org-format-latex-header
   "\\documentclass{article}\12\\usepackage[usenames]{color}\12\\usepackage{bm}\12[DEFAULT-PACKAGES]\12[PACKAGES]\12\\pagestyle{empty}             % do not remove\12% The settings below are copied from fullpage.sty\12\\setlength{\\textwidth}{\\paperwidth}\12\\addtolength{\\textwidth}{-3cm}\12\\setlength{\\oddsidemargin}{1.5cm}\12\\addtolength{\\oddsidemargin}{-2.54cm}\12\\setlength{\\evensidemargin}{\\oddsidemargin}\12\\setlength{\\textheight}{\\paperheight}\12\\addtolength{\\textheight}{-\\headheight}\12\\addtolength{\\textheight}{-\\headsep}\12\\addtolength{\\textheight}{-\\footskip}\12\\addtolength{\\textheight}{-3cm}\12\\setlength{\\topmargin}{1.5cm}\12\\addtolength{\\topmargin}{-2.54cm}\12\\newcommand{\\f}[2]{\\frac{#1}{#2}}\12\\newcommand{\\bb}[1]{\\mathbb{#1}}\12\\newcommand{\\up}[2]{{#1}^{#2}}\12\\newcommand{\\cal}[1]{\\mathcal{#1}}\12\\newcommand{\\fk}[1]{\\mathfrak{#1}}")
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.5 :html-foreground
		 "Black" :html-background "Transparent" :html-scale
		 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")
		 :use-image-magick t))
 '(org-latex-packages-alist '(("" "tikz-cd" t nil) ("" "tikz" t nil)))
 '(org-preview-latex-default-process 'dvisvgm)
 '(package-selected-packages
   '(avy capf-autosuggest company-box cond-let counsel dashboard
	 dired-preview doom-modeline dracula-theme eldoc-box
	 esh-autosuggest esh-help eshell-autojump eshell-git-prompt
	 eshell-syntax-highlighting evil-collection evil-leader
	 flycheck lsp-ui mason multiple-cursors org-modern pdf-tools
	 reformatter rust-mode slime texfrag typescript-mode vertico
	 with-editor xterm-color)))
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
(load (concat custom-config-dir "my_fill.el"))
(load (concat custom-config-dir "eshell.el"))
;;(load (concat custom-config-dir "lsp.el"))

(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)
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

;;(require 'dashboard)
;;(dashboard-setup-startup-hook)
;;(setq dashboard-banner-logo-title "EVIL")
;;;;;; 可选：自定义显示哪些项目（例如最近文件和项目）
;;(setq dashboard-items '((recents . 10)
;;			(bookmarks . 5))) ; 显示 5 个待办事项
;;(setq dashboard-center-content t)
;;(setq dashboard-vertically-center-content t)
(use-package dashboard
  :ensure t
  :config
    (dashboard-setup-startup-hook)
    (setq dashboard-banner-logo-title "EVIL")
    ;;;; 可选：自定义显示哪些项目（例如最近文件和项目）
    (setq dashboard-items '((recents . 10)
			    (bookmarks . 5))) ; 显示 5 个待办事项
    ;;(setq dashboard-center-content t)
    (setq dashboard-vertically-center-content t)
  )

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

(use-package avy
  :ensure t
  )

(use-package eldoc-box
  :ensure t)
(setq lsp-meson-no-auto-downloads t)

(use-package dired-preview
  :ensure t
  :config
(setq dired-preview-delay 0.05)

  ;; 右侧预览窗口
  (setq dired-preview-display-action-alist
        '((display-buffer-in-side-window)
          (side . right)
          (window-width . 0.5)))
  ;(dired-preview-global-mode 1)
)
;(use-package multiple-cursors
;  :ensure t
;  :bind
;   (("C-S-c C-S-c" . mc/edit-lines)          ;; 多行同列编辑
;   ("C->"         . mc/mark-next-like-this) ;; 向下找相同
;   ("C-<"         . mc/mark-previous-like-this) ;; 向上找相同
;   ("C-c C-<"     . mc/mark-all-like-this)  ;; 选中全部相同
;   ("C-c C-c"     . mc/edit-lines)))

(setq auto-save-default t)
(setq auto-save-timeout 5)
(setq auto-save-interval 50)
