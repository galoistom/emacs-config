;; Scheme
(use-package scheme
  :ensure nil)

(use-package geiser
  :ensure t
  :config
  (setq geiser-repl-history-filename "~/.emacs.d/geiser-history"))

(use-package geiser-guile
  :ensure t)
