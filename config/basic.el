(use-package avy :ensure t)
(use-package god-mode :ensure t :config (global-set-key (kbd "C-c g") #'god-local-mode))
(use-package magit :ensure t :bind ("C-x g" . magit-status))
(use-package eldoc-box :ensure t)
(use-package counsel :ensure t)
(use-package xdg-launcher :ensure t)
(use-package dash :ensure t)
(use-package s :ensure t)
(use-package f :ensure t)
(use-package kkp :ensure t :config (global-kkp-mode 1))
;;(setq lsp-meson-no-auto-downloads t)
(use-package multiple-cursors :ensure t)
(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (ansi-color-apply-on-region (point-min) (point-max)))
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project) 
  (setq doom-modeline-minor-modes nil))
;better support for barkets, especially for elisp
(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode conf-mode yaml-mode) . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5))
(set-face-attribute 'show-paren-match nil
                    :weight 'bold
                    :background "#44475a"
                    :foreground "#ffff00")
;help use keybdings
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-max-description-length 40)
  (setq which-key-max-display-columns nil))
;better search
(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
;  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-M-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))
;joining symbols
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

(add-to-list 'load-path "/home/galoistom/emskin/elisp")
(require 'emskin)
