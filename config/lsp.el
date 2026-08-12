;;; -*- lexical-binding: t; -*-
(use-package corfu
  :ensure t
  :init (global-corfu-mode)
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


(use-package eglot
  :ensure nil
  :config
  (setq eglot-autoshutdown t))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'")
(declare-function gofmt "go-mode")
(use-package go-dlv :ensure t)
;; (add-hook 'go-ts-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook #'gofmt nil t)))

(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))
