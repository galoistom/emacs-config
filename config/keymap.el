
(defun qutebrowser (url)
  (interactive "sinput url:")
  (start-process-shell-command "browser" nil (format "qutebrowser %s" url)))

(defun my/cheatsheet (name)
  (interactive "ssearch cheatsheet: ")
  (compile (format "curl cheat.sh/%s" name)))

(defun my/capital-forward ()
  (interactive)
  (backward-word)
  (capitalize-word 1))

(define-minor-mode my-cj-mode
  "Force C-j to be C-x map."
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-j")     ctl-x-map)
	    (define-key map (kbd "C-c C-d") #'backward-kill-word)
	    (define-key map (kbd "C-c d")   #'kill-word)
            map))

(my-cj-mode 1)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c C-e") 'wdired-change-to-wdired-mode))

(define-prefix-command 'my/w-prefix)
(global-set-key (kbd "C-c w") 'my/w-prefix)
(global-set-key (kbd "C-c w s") #'split-window-below)
(global-set-key (kbd "C-c w v") #'split-window-right)
(global-set-key (kbd "C-c w d") #'kill-buffer-and-window)
(global-set-key (kbd "C-c q") #'delete-other-windows)
(global-set-key (kbd "C-c w n") #'balance-windows)
(global-set-key (kbd "C-c w m") #'maximize-window)

(global-set-key (kbd "C-.")          #'duplicate-line)
(global-set-key (kbd "C-v")          #'my-fill-function)
(global-set-key (kbd "s-s")          #'save-buffer)
(global-set-key (kbd "s-d")          #'backward-delete-char)
(global-set-key (kbd "s-f")          #'ido-find-file)
(global-set-key (kbd "s-<return>")   #'org-meta-return)
(global-set-key (kbd "C-<tab>")      #'other-window)
(global-set-key (kbd "M-\"")         #'shell-command)
(global-set-key (kbd "C-M-n")        #'mc/mark-next-like-this)
(global-set-key (kbd "C-M-p")        #'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-f")        #'up-list)
(global-set-key (kbd "M-e")          #'mark-word)

(global-set-key (kbd "C-x j")        #'flash-emacs-jump)
(global-set-key (kbd "C-x k")        #'goto-last-change)
(global-set-key (kbd "C-x C-a")      #'replace-regexp)
(global-set-key (kbd "C-x c")        #'compile)

(global-set-key (kbd "C-c b")        #'qutebrowser)
(global-set-key (kbd "C-c e")        #'vterm)
(global-set-key (kbd "C-c z")        #'zap-to-char)
(global-set-key (kbd "C-c c")        #'my/capital-forward)
(global-set-key (kbd "C-c r")        #'rgrep)
(global-set-key (kbd "C-c D")        #'kill-whole-line)
(global-set-key (kbd "C-c i")        #'mc/edit-lines)
(global-set-key (kbd "C-c j")        #'mc/unmark-previous-like-this)
(global-set-key (kbd "C-c k")        #'mc/unmark-next-like-this)
(global-set-key (kbd "C-c l")        #'xdg-launcher-run-app)
(global-set-key (kbd "C-c F")        #'eglot-format-buffer)
(global-set-key (kbd "C-c R")        #'eglot-reconnect)
(global-set-key (kbd "C-c m")        #'my-latex-math-auto-fill-mode)
(global-set-key (kbd "C-c p")        #'math-preview-all)
(global-set-key (kbd "C-c P")        #'math-preview-clear-all)
(global-set-key (kbd "C-c S")        #'my/cheatsheet)
(global-set-key (kbd "C-c s")        #'swiper)
(global-set-key (kbd "C-c f")        #'counsel-fzf)
(global-set-key (kbd "C-c C-l")      #'eglot)
