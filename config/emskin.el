;;; -*- lexical-binding: t; -*-
;;; code:
(add-to-list 'load-path "/home/galoistom/emskin/elisp")
(add-to-list 'load-path "/home/galoistom/Projects/minecraft.el")
(require 'emskin)
(require 'minecraft)
(require 'fzf)

(defmacro my/emskin-key (key name)
  "Customize keybindings for emskin, KEY for key, NAME for start command."
  `(global-set-key (kbd ,key) (cons (car (last (split-string ,name)))
                                    (lambda () (interactive) (emskin-open-native-app ,name)))))
    
(define-prefix-command 'my/emskin-prefix)
(global-set-key (kbd "C-c o") 'my/emskin-prefix)
(setq browser "firefox")
(my/emskin-key "C-c o b" browser)
(my/emskin-key "C-c o k" "kitty")
(my/emskin-key "C-c <return>" "kitty")
(my/emskin-key "C-c o m" "kitty rmpc")
(my/emskin-key "C-c o n" "env GTK_THEME=Adwaita-dark thunar")
(my/emskin-key "C-c o q" "qutebrowser")
(my/emskin-key "C-c o p" (concat browser " --private-window"))

(defun my/xdg-open ()
  "Fzf file to open with xdg."
  (interactive)
  (fzf-with-command
   "find /home/galoistom -type f -not -path '*/.*'"
   (lambda (file)
     (when file
       (emskin-open-native-app (format "xdg-open \"%s\"" file))))))

(global-set-key (kbd "C-c o f") '("fzf-open" . my/xdg-open))


(defun my/emskin-open ()
  "Open apps in emskin with complition."
  (interactive)
  (let ((input (read-shell-command "emskin-open: ")))
    (emskin-open-native-app input)))

(global-set-key (kbd "C-c C-k") #'my/emskin-open)
