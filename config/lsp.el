(use-package lsp-mode
  :ensure t
  :init
  ;; 推荐：全局启用 lsp-mode 的某些功能
  (setq lsp-header-section-enable nil) ; 禁用文件顶部的 LSP 信息
  (setq lsp-enable-diagnostics t)
  (setq lsp-enable-snippet nil)        ; 禁用内置代码片段，通常配合 yasnippet使用
  (setq lsp-auto-configure t)          ; 自动配置 LSP 服务器
  (setq lsp-enable-server-auto-start t) ; 发现缺失的服务器时提示安装
  :hook
  (lsp-mode . lsp-enable-flycheck)
  (lsp-mode . lsp-enable-which-key-integration)

  )
(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
;;  :hook
;;  (lsp-mode . lsp-ui-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-enable t) ; 启用文档浮动窗口 (Hovering)
  (setq lsp-ui-sideline-enable t) ; 开启侧边栏信息 (Side-line)
;;  ;; 配置 Side-line 提示
  (setq lsp-ui-sideline-show-hover t) ; 鼠标悬停时显示更多信息
  (setq lsp-ui-sideline-ignore-duplicate nil) ; 显示重复错误
)
(use-package flycheck
  :ensure t
  :config
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行
(global-flycheck-mode)
  :hook
  (prog-mode . flycheck-mode))
(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1) ; 只需敲 1 个字母就开始进行自动补全
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t) ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence)))
(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode))

(use-package vertico
  :ensure t
  :init
  ;; 启用 Vertico 模式
  (vertico-mode)
  )

;; -------------------------------------------
;; SLIME (Superior Lisp Interaction Mode) 配置
;; -------------------------------------------

(require 'slime)

(setq slime-lisp-implementations
     '((sbcl ("sbcl"))))

(global-set-key (kbd "C-c l") 'slime)
