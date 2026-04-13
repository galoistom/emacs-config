;;; -*- lexical-binding: t; -*-
;; (use-package lsp-mode
;;   :ensure t
;;   :init
;;   ;; 推荐：全局启用 lsp-mode 的某些功能
;;   (setq lsp-enable-diagnostics t)
;;   (setq lsp-auto-configure t)          ; 自动配置 LSP 服务器
;;   (setq lsp-enable-server-auto-start t) ; 发现缺失的服务器时提示安装
;;   :hook
;;   (lsp-mode . lsp-enable-flycheck)
;;   (lsp-mode . lsp-enable-which-key-integration)
;;   ;(go-ts-mode . lsp-deferred)
;;   )
;; (use-package lsp-ui
;;   :ensure t
;;   :config
;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;;   (setq lsp-ui-doc-position 'top)
;;   (setq lsp-ui-doc-enable t) ; 启用文档浮动窗口 (Hovering)
;;   (setq lsp-ui-sideline-enable t) ; 开启侧边栏信息 (Side-line)
;;   (setq lsp-ui-sideline-show-hover t) ; 鼠标悬停时显示更多信息
;;   (setq lsp-ui-sideline-ignore-duplicate nil) ; 显示重复错误
;; )

;; (use-package company
;;   :ensure t
;;   :init (global-company-mode)
;;   :config
;;   (setq company-minimum-prefix-length 2) ; 只需敲 1 个字母就开始进行自动补全
;;   (setq company-tooltip-align-annotations t)
;;   (setq company-idle-delay 0.0)
;;   (setq company-show-numbers t) ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
;;   (setq company-selection-wrap-around t)
;;   (setq company-transformers '(company-sort-by-occurrence)))

;; (use-package company-box
;;   :ensure t
;;   :if window-system
;;   :hook (company-mode . company-box-mode))

(use-package corfu
  :ensure t
  :init
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.2)
  (setq corfu-count 5)
  (setq corfu-auto-prefix 1)
  (setq corfu-cycle t))

(use-package corfu-terminal
  :ensure t
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list
                         #'cape-dabbrev
                         #'cape-file))))

;;;; Completion sources
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package flycheck
  :ensure t
  :config
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行
  (global-flycheck-mode)
  :hook
  (prog-mode . flycheck-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides
        '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t)

(use-package eglot
  :ensure nil
  ;; :hook
  ;; (go-ts-mode . eglot-ensure)
  :config
  (setq eglot-autoshutdown t))

(use-package vertico
  :ensure t
  :init
  ;; 启用 Vertico 模式
  (vertico-mode))

(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'"
  :hook (go-ts-mode . (lambda ()
                        (setq tab-width 8)
                        (setq indent-tabs-mode 1))))
(declare-function gofmt "go-mode")
(add-hook 'go-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'gofmt nil t)))

(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))
