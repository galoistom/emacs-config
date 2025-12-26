(use-package eshell-git-prompt
  :ensure t
  :after esh-mode
)
(use-package eshell-syntax-highlighting
  :after eshell-mode
  :ensure t ;; Install if not already installed.
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))
(use-package capf-autosuggest
  :ensure t
  :hook ((eshell-mode comint-mod) . capf-autosuggest-mode)
  :config
    (define-key eshell-mode-map (kbd "<right>") 'capf-autosuggest-move-end-of-line)
  )

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
	      ("C-r" . consult-history))
  :config
  (with-eval-after-load 'evil
			(evil-define-key '(normal insert) eshell-mode-map (kbd "C-r") #'consult-history)))
