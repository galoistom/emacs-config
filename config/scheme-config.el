;;; -*- lexical-binding: t; -*-
;; Scheme
;; 删除所有 .rkt 的关联
;; 再删除可能存在的其他关联
(setq auto-mode-alist
      (assq-delete-all "\\.rkt\\'" auto-mode-alist))

;; 如果仍有问题，直接覆盖
(setq auto-mode-alist
      (cons '("\\.rkt\\'" . racket-mode)
            (assq-delete-all "\\.rkt\\'" auto-mode-alist)))
(use-package scheme
  :ensure nil
  :config
  (remove-hook 'auto-mode-alist '("\\.rkt\\'" . scheme-mode)))
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(use-package racket-mode
  :ensure t
  :mode ("\\.rkt\\'" . racket-mode)
  :hook
  (racket-mode . (lambda () (flycheck-mode -1)))
  (racket-mode . racket-xp-mode)
  :config
  (with-eval-after-load 'compile
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(racket "^\\([a-zA-Z0-9_./-]+\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3))
    (add-to-list 'compilation-error-regexp-alist 'racket)))

(use-package geiser
  :ensure t
  :config
  (setq geiser-repl-history-filename "~/.emacs.d/geiser-history"))

(use-package geiser-guile
  :ensure t)
