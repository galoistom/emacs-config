;;; -*- lexical-binding: t; -*-
;; Scheme
(use-package scheme
  :ensure nil)

(use-package racket-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))

(use-package geiser
  :ensure t
  :config
  (setq geiser-repl-history-filename "~/.emacs.d/geiser-history"))

(use-package geiser-guile
  :ensure t)

(use-package geiser-racket
  :ensure t)
