;;; -*- lexical-binding: t; -*-
;; Scheme
;; 删除所有 .rkt 的关联
;; 再删除可能存在的其他关联
(setq auto-mode-alist
      (assq-delete-all "\\.rkt\\'" auto-mode-alist))
;; 强制 racket-mode 关联 .rkt
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))

;; 如果仍有问题，直接覆盖
(setq auto-mode-alist
      (cons '("\\.rkt\\'" . racket-mode)
            (assq-delete-all "\\.rkt\\'" auto-mode-alist)))
(use-package scheme
  :ensure nil
  :config
  (remove-hook 'auto-mode-alist '("\\.rkt\\'" . scheme-mode)))

(use-package racket-mode
  :ensure t
  :mode ("\\.rkt\\'" . racket-mode))

(use-package geiser
  :ensure t
  :config
  (setq geiser-repl-history-filename "~/.emacs.d/geiser-history"))

(use-package geiser-guile
  :ensure t)

(use-package geiser-racket
  :ensure t)
