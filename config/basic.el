;;; -*- lexical-binding: t; -*-
(use-package avy :ensure t)
(use-package magit :ensure t :bind ("C-x g" . magit-status))
(use-package eldoc-box :ensure t)
;; (use-package counsel :ensure t)
(use-package xdg-launcher :ensure t)
(use-package undo-tree :ensure t)
(use-package dash :ensure t)
(use-package s :ensure t)
(use-package f :ensure t)
(use-package kkp :ensure t :config (global-kkp-mode 1))
(use-package multiple-cursors :ensure t)
(use-package ghostel :ensure t)
(use-package fzf :ensure t)
(require 'ansi-color)
(require 'dired)
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
     (t (dired-find-file-other-window)))))
(define-key dired-mode-map (kbd "o") 'my/dired-open-file-other-window)

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
  (setq doom-modeline-minor-modes nil))

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
  :ensure t
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
