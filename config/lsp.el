;;; -*- lexical-binding: t; -*-
(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.5)
  (setq corfu-count 5)
  (setq corfu-auto-prefix 1)
  (setq corfu-cycle t))

(use-package nerd-icons-corfu
  :after corfu
  :ensure t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

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
  (setq truncate-lines nil)
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


(defun my/eglot-disable-flycheck ()
  "Disable Flycheck in Eglot-managed buffers.
Eglot reports diagnostics through Flymake, so keeping both enabled
would run two syntax checkers on the same buffer."
  (when (bound-and-true-p flycheck-mode)
    (flycheck-mode -1)))

(use-package eglot
  :ensure nil
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (eglot-managed-mode . my/eglot-disable-flycheck))
  :config
  (setq eglot-autoshutdown t)
  ;; pyright 通过 npm 全局安装，eglot 默认已支持，这里显式指定。
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("pyright-langserver" "--stdio"))))

(use-package go-ts-mode
  :ensure t
  :mode "\\.go\\'")
;; (declare-function gofmt "go-mode")
(use-package go-dlv :ensure t)
(add-hook 'go-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'gofmt nil t)))

(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-font-lock-level 4)
  (global-treesit-auto-mode))

(add-to-list 'load-path "/home/galoistom/emacs-config/site-elisp/promela-mode/") ; location where you cloned promela-mode
(require 'promela-mode)
(add-to-list 'auto-mode-alist '("\\.pml\\'" . promela-mode))
