;;; -*- lexical-binding: t; -*-
(use-package avy :ensure t)
(use-package transient :ensure t)
(use-package magit :after transient :ensure t :bind ("C-x g" . magit-status))
(use-package eldoc-box :ensure t)
(use-package xdg-launcher :ensure t)
(use-package dash :ensure t)
(use-package s :ensure t)
(use-package f :ensure t)
(use-package kkp :ensure t :config (global-kkp-mode 1))
(use-package nerd-icons :ensure t)
(use-package fzf :ensure t)
(require 'ansi-color)
(require 'corfu)
(require 'dired)
(use-package nerd-icons-dired :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package ghostel
  :ensure t
  :config
  (define-key ghostel-mode-map (kbd "C-c t") (lambda () (interactive) (ghostel t))))

(use-package nerd-icons-completion :ensure t
  :config (nerd-icons-completion-mode 1))

(use-package multiple-cursors :ensure t
  :config
  (defvar my/mc-saved-cut-function nil)
  (defvar my/mc-saved-paste-function nil)
  (defvar my/mc-saved-select-enable-clipboard nil)
  (defvar my/mc-clipboard-disabled-p nil)
  (defun my/mc-toggle-clipboard ()
    (if multiple-cursors-mode
        (unless my/mc-clipboard-disabled-p
          (setq my/mc-saved-cut-function interprogram-cut-function
                my/mc-saved-paste-function interprogram-paste-function
                my/mc-saved-select-enable-clipboard select-enable-clipboard)
          (setq interprogram-cut-function nil
                interprogram-paste-function nil
                select-enable-clipboard nil)
          (setq my/mc-clipboard-disabled-p t))
      (when my/mc-clipboard-disabled-p
        (setq interprogram-cut-function my/mc-saved-cut-function
              interprogram-paste-function my/mc-saved-paste-function
              select-enable-clipboard my/mc-saved-select-enable-clipboard)
        (setq my/mc-clipboard-disabled-p nil))))
  :hook (multiple-cursors-mode . my/mc-toggle-clipboard))

(defun my/ansi-colorize-buffer ()
  "Colorize buffer."
  (ansi-color-apply-on-region (point-min) (point-max)))
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(defun my/dired-open-file-other-window ()
  "Customized open DIRED file."
  (interactive)
  (let* ((file (dired-get-file-for-visit)))
    (cond
     ((string-match "\\.pdf\\'" file) (call-process "okular" nil 0 nil file))
     ((string-match "\\.mp4\\'" file) (call-process "mpv" nil 0 nil file))
     ((string-match "\\.epub\\'" file) (call-process "ebook-viewer" nil 0 nil file))
     (t (dired-find-file-other-window)))))
(define-key dired-mode-map (kbd "o") 'my/dired-open-file-other-window)

;;better support for barkets, especially for elisp
(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode conf-mode yaml-mode) . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5))
(set-face-attribute 'show-paren-match nil
                    :weight 'bold
                    :background "#44475a"
                    :foreground "#ffff00")

;;help use keybdings
(use-package which-key
  :ensure nil
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-max-description-length 40)
  (setq which-key-max-display-columns nil))

(use-package consult
  :ensure t
  :config
  (setq consult-preview-key 'any)
  (recentf-mode t))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;;joining symbols
(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 't
    '("www" "**" "***" "**/" "*>" "*/" "\\\\" "||"
      "==" "===" "!=" "!==" "=/=" "<=" ">=" "<=>" "=>"
      "->" "<-" "->>" "<<-" "<-<" ">>-" "-<" "-<<"
      "<<<" ">>>" "<|" "|>" "<||" "||>" "<|||" "|||>"
      "<$" "$>" "<$>" "<+" "+>" "<+>" "<*" "*>"
      "</" "</>" "/>" "<!--" "<!---" "<==>" "<=="
      "<=>" "==>" "=>>" ">=>" ">>=" "=:=" "=!=" "==!="))
  (global-ligature-mode t))

(use-package flash-emacs
  :vc (:url "https://github.com/JiaweiChenC/flash-emacs"))
