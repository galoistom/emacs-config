;;; -*- lexical-binding: t; -*-
(use-package eshell-git-prompt
  :ensure t
  :after esh-mode)

(use-package eshell-syntax-highlighting
  :ensure t
  :after esh-mode
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package em-history
  :ensure nil
  :defer t
  :custom
  (eshell-history-size 1024)
  (eshell-his-ignoredups t)
  (eshell-save-history-on-exit t))

(use-package esh-mode
  :ensure nil
  :config
  (define-key eshell-mode-map (kbd "C-r") #'consult-history)
  (define-key eshell-mode-map (kbd "C-c e") (lambda () (interactive) (eshell t))))
