;;; -*- lexical-binding: t; -*-
;; Scheme
(use-package scheme
  :ensure nil)

(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))

(use-package geiser
  :ensure t
  :config
  (setq geiser-repl-history-filename "~/.emacs.d/geiser-history"))
(use-package geiser-guile
  :ensure t)
