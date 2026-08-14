;;; -*- lexical-binding: t; -*-
(use-package eshell-git-prompt
  :ensure t
  :after esh-mode)

(use-package eshell-syntax-highlighting
  :after eshell-mode
  :ensure t
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(with-eval-after-load 'eshell
  (define-key eshell-mode-map (kbd "C-c e")
              (lambda ()
                (interactive)
                (eshell t))))
(use-package em-history
  :ensure nil
  :defer t
  :custom
  (eshell-history-size 1024)
  (eshell-his-ignoredups t)
  (eshell-save-history-on-exit t))

(use-package esh-mode
  :ensure nil
  :hook (eshell-mode . (lambda ()
			 (local-set-key (kbd "C-r") #'consult-history)))
  :bind (:map eshell-mode-map
	      ("C-r" . consult-history)))
