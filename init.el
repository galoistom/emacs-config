;; 初始化 EMACS 内置的包管理系统
(setq url-proxy-services
      '(("http"  . "127.0.0.1:7890")
        ("https" . "127.0.0.1:7890")))
(set-fontset-font (frame-parameter nil 'font) 
                  'unicode 
                  (font-spec :family "Hack Nerd Font")) 
(set-face-attribute 'default nil 
  :family "Cascadia Code"   ; 替换为你想要的主字体
  :height 120                ; 设置字体大小，120 等于 12pt
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
   '("4dcf06273c9f5f0e01cea95e5f71c3b6ee506f192d75ffda639d737964e2e14e"
     default))
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(package-selected-packages
   '(catppuccin-theme company dashboard doom-modeline evil-collection
		      evil-leader lsp-ui macrostep mason math-preview
		      pdf-tools rust-mode texfrag vertico xterm-color)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; -------------------------------------------
;; Evil Mode 配置
;; -------------------------------------------
(setq custom-config-dir (expand-file-name "config/" user-emacs-directory))
(load (concat custom-config-dir "evil.el"))
(load (concat custom-config-dir "markdown.el"))
;;(load (concat custom-config-dir "lsp.el"))

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
;; -------------------------------------------
;; SLIME (Superior Lisp Interaction Mode) 配置
;; -------------------------------------------

;; 1. 加载 slime 扩展
(require 'slime)

;; 2. 指定你的 Lisp 实现 (这里是 SBCL)
;; 如果你将 SBCL 安装在 /usr/local/bin/，通常无需配置路径，但以防万一：
(setq slime-lisp-implementations
     '((sbcl ("sbcl"))))

(global-set-key (kbd "C-c l") 'slime)
;; 2. 指定你的 Lisp 实现 (这里是 SBCL)
;; 如果你将 SBCL 安装在 /usr/local/bin/，通常无需配置路径，但以防万一：

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(set-frame-parameter (selected-frame) 'background-mode 'dark) 
;;(set-face-attribute 'default nil 
;;                    :background "#000000")
(setq xterm-query-color-support t)
(unless (display-graphic-p)
  (setq-default frame-background-mode nil)
  (setq term-setup-hook (lambda ()
                          (set-terminal-parameter nil 'frame-background-mode 'dark))))
(load-theme 'catppuccin :no-confirm)

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

;;(math-preview-on-change t)
;; 将函数添加到 after-save-hook 钩子
;;(add-hook 'after-save-hook 'my-math-preview-on-save)
;;(add-hook 'markdown-mode-hook 'markdown-table-mode)
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  ;; 隐藏窗口两侧的空白边距，使模式行更紧凑
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project) 
  
  ;; 隐藏次要模式（Minor Modes）名称，如 "vc" 或 "abbrev"，使模式行更简洁
;;  (setq doom-modeline-minor-modes nil) 
  
  ;; 设置 Modelina 的高度（如果觉得默认太高或太矮）
 ;; (setq doom-modeline-height 25) 
(setq doom-modeline-minor-modes nil)
  
)
(mason-ensure t)

(use-package lsp-mode
  :ensure t
  :init
  ;; 推荐：全局启用 lsp-mode 的某些功能
  (setq lsp-header-section-enable nil) ; 禁用文件顶部的 LSP 信息
  (setq lsp-enable-snippet nil)        ; 禁用内置代码片段，通常配合 yasnippet使用
  (setq lsp-auto-configure t)          ; 自动配置 LSP 服务器
  (setq lsp-enable-server-auto-start t) ; 发现缺失的服务器时提示安装
  )
;;(use-package lsp-ui)


(use-package company
  :ensure t
  :config
  ;; 全局启用 company-mode (代码补全)
  (global-company-mode t)
  
  ;; 设置 company 补全提示的延迟和最小字符数
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 1)
  )

(use-package vertico
  :ensure t
  :init
  ;; 启用 Vertico 模式
  (vertico-mode)
  
  ;; 推荐：启用 Vertico 的一些辅助功能
   ;;(vertico-multiform-mode) 
   ;;(vertico-directory-exhibit-mode)
  )
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

