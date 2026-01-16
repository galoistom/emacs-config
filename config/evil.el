(setq evil-want-keybinding nil)
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (dolist (mode '(eww-mode
                  help-mode
                  info-mode
                  Man-mode
                  customize-group-mode
                  customize-mode
                  package-menu-mode))
    (evil-set-initial-state mode 'emacs))
    (global-set-key (kbd "C-s") #'avy-goto-char)
    (evil-define-key 'normal 'global (kbd "s") 'avy-goto-char)
  )

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  ;; 设置显示延迟（毫秒）
  (setq which-key-idle-delay 0.5)
  ;; 设置显示位置
  (setq which-key-side-window-location 'bottom)
  ;; 最大化显示宽度
  (setq which-key-max-description-length 40)
  (setq which-key-max-display-columns nil)
)

(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-leader
  :ensure t
  :config
    (evil-leader/set-leader "<SPC>") 
    (evil-leader/set-key
    "s" 'save-buffer      
    "q" 'save-buffers-kill-terminal 
    "f" 'counsel-find-file        
    "e" 'dired-jump
    "h" 'dashboard-open
    "t" 'vterm
    "F" 'bookmark-jump
    "p" 'dired-preview-mode
    "n" 'eshell
    ))

;(use-package evil-mc
;  :ensure t
;  :after evil
;  :config
;  (evil-leader/set-key "k" evil-mc-cursors-map)
;  (global-evil-mc-mode 1))

(global-evil-leader-mode)
(setq evil-default-state 'normal)

(defmacro my/define-keys (keymap &rest bindings)
  `(progn
     ,@(mapcar (lambda (b)
		 `(define-key ,keymap (kbd ,(car b)) ',(cadr b))) bindings)))

(define-prefix-command 'my-window-map)
(evil-leader/set-key "w" 'my-window-map)
(my/define-keys my-window-map
		("h" windmove-left)
		("j" windmove-down)
		("k" windmove-up)
		("l" windmove-right)
		("%" split-window-right)
		("\"" split-window-below)
		("x" delete-window)
		("o" delete-other-windows)
		("H" shrink-window-horizontally)
		("L" enlarge-window-horizontally)
		("J" shrink-window)
		("K" enlarge-window)
		("r" evil-window-rotate-downwards)
		("R" evil-window-rotate-upwards)
		("s" window-swap-states)
		("m" maximize-window)
		("1" balance-windows))

(define-prefix-command 'my-org-keymap)
(evil-leader/set-key "o" 'my-org-keymap)
(my/define-keys my-org-keymap
		("v" org-toggle-inline-images)
		("c" org-latex-preview)
		("r" org-ctrl-c-ctrl-c)
		("o" org-open-at-point))

(define-prefix-command 'my-buffer-action)
(evil-leader/set-key "b" 'my-buffer-action)
(my/define-keys my-buffer-action
		("x" kill-buffer-and-window)
		("X" kill-buffer)
		("l" buffer-menu)
		("n" previous-buffer)
		("p" next-buffer)
		("b" ivy-switch-buffer))

(define-prefix-command 'my-slime-keymap)
(evil-leader/set-key "g" 'my-slime-keymap)
(my/define-keys my-slime-keymap
		("s" slime)
		("k" slime-compile-and-load-file)
		("q" slime-quit)
		("b" eval-buffer)
		("e" eval-last-sexp))

(define-prefix-command 'my-latex-render)
(evil-leader/set-key "l" 'my-latex-render)
(my/define-keys my-latex-render
		("p" preview-at-point)
		("t" texfrag-docuemnt))

(defun qutebrowser (url)
  (interactive "sinput url:")
  (start-process-shell-command "browser" nil (format "qutebrowser %s" url)))

(evil-leader/set-key "u" 'qutebrowser)
(evil-leader/set-key "r" 'eww)

(with-eval-after-load 'wdired
  (define-key wdired-mode-map (kbd "C-k") #'wdired-finish-edit))

(setq evil-undo-system 'undo-fu)

(require 'myfill)
(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "C-k") #'kill-line)
  (define-key evil-insert-state-map (kbd "C-o") #'org-meta-return)
  (define-key evil-insert-state-map (kbd "C-d") #'delete-char)
  (define-key evil-insert-state-map (kbd "C-v") #'forward-char)
  (define-key evil-insert-state-map (kbd "C-b") #'backward-char)
  (define-key evil-insert-state-map (kbd "C-n") #'next-line)
  (define-key evil-normal-state-map (kbd "C-n") #'next-line)
  (define-key evil-insert-state-map (kbd "C-p") #'previous-line)
  (define-key evil-normal-state-map (kbd "C-p") #'previous-line)
  (define-key evil-insert-state-map (kbd "C-a") #'move-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") #'move-end-of-line)
  (define-key evil-normal-state-map (kbd "C-e") #'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-f") #'myfill)
  (evil-define-key '(normal insert visual motion) 'global (kbd "C-j") ctl-x-map)
  (global-set-key (kbd "C-,") #'duplicate-line)
  )

(with-eval-after-load 'org
  (with-eval-after-load 'evil
    (evil-define-key '(normal motion visual insert)
        org-mode-map
      (kbd "C-j") ctl-x-map)))
