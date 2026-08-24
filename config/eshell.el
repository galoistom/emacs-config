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

(require 'consult)
(use-package esh-mode
  :ensure nil
  :config
  (define-key eshell-mode-map (kbd "C-t") #'consult-history)
  (define-key eshell-mode-map (kbd "C-c e") (lambda () (interactive) (eshell t))))

(defun my-eshell-view-file (file)
  "View FILE.  A version of `view-file' which properly rets the eshell prompt."
  (interactive "fView file: ")
  (unless (file-exists-p file) (error "%s does not exist" file))
  (let ((buffer (find-file-noselect file)))
    (if (eq (get (buffer-local-value 'major-mode buffer) 'mode-class)
            'special)
        (progn
          (switch-to-buffer buffer)
          (message "Not using View mode because the major mode is special"))
      (let ((undo-window (list (window-buffer) (window-start)
                               (+ (window-point)
                                  (length (funcall eshell-prompt-function))))))
        (switch-to-buffer buffer)
        (view-mode-enter (cons (selected-window) (cons nil undo-window))
                         'kill-buffer)))))
