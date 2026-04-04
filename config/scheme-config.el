;;; -*- lexical-binding: t; -*-
;; Scheme
(use-package scheme
  :ensure nil)

(use-package geiser
  :ensure t
  :config
  (setq geiser-repl-history-filename "~/.emacs.d/geiser-history"))

(use-package geiser-guile
  :ensure t)
;; (setq geiser-repl-query-on-exit-p nil) ;; 退出时不询问
;; ;; 或者强制 Geiser 在没有 REPL 时静默
;; (advice-add 'geiser-capf--for-symbol :around
;;             (lambda (orig-fun &rest args)
;;               (ignore-errors (apply orig-fun args))))
;; (defun my-scheme-mode-setup ()
;;   "仅在 Geiser REPL 激活时才添加补全后端。"
;;   (setq-local completion-at-point-functions
;;               (list (lambda ()
;;                       (when (and (bound-and-true-p geiser-repl--connection)
;;                                  (geiser-repl--connection))
;;                         (geiser-capf--for-symbol))))))

;; (add-hook 'scheme-mode-hook #'my-scheme-mode-setup)
;; (with-eval-after-load 'geiser-capf
;;   (advice-add 'geiser-capf--for-symbol :around
;;               (lambda (orig-fun &rest args)
;;                 (condition-case nil
;;                     (apply orig-fun args)
;;                   (error nil))))) ;; 捕获所有错误并返回 nil，让 Corfu 换下一个后端
