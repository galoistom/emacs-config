;;; -*- lexical-binding: t; -*-
(add-to-list 'load-path "/home/galoistom/emskin/elisp")
(require 'emskin)

;; 在外部配置文件中
(with-eval-after-load 'my-cj-mode
    (define-key (make-sparse-keymap) (kbd "C-c C-k") #'emskin-open-native-app))

(define-prefix-command 'my/o-prefix)
(global-set-key (kbd "C-c o") 'my/o-prefix)
(global-set-key (kbd "C-c o b")  (lambda () (interactive) (emskin-open-native-app "qutebrowser")))
(global-set-key (kbd "C-c o k")  (lambda () (interactive) (emskin-open-native-app "kitty")))
(global-set-key (kbd "C-c o n")  (lambda () (interactive) (emskin-open-native-app "kitty -e zsh -i -c nnn")))
(global-set-key (kbd "C-c o o")  (lambda () (interactive) (emskin-open-native-app "rofi -show combi")))
