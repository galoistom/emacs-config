(setq evil-want-keybinding nil)
(use-package evil
  :ensure t
  :config
    (evil-mode 1)
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

(use-package evil-mc
  :ensure t
  :after evil
  :config
  (global-evil-mc-mode 1))

(use-package evil-leader
  :ensure t
  :config
    (evil-leader/set-leader "<SPC>") 
    (evil-leader/set-key
    "s" 'save-buffer      ; <Space> s -> 保存文件
    "q" 'save-buffers-kill-terminal ; <Space> q -> 保存所有并退出 Emacs
    "f" 'counsel-find-file        ; <Space> f -> 查找文件 (C-x C-f 替代)
    "e" 'dired-jump
    "h" 'dashboard-open
    "t" 'eshell
    "F" 'bookmark-jump
    "p" 'dired-preview-mode
    )
)


(global-evil-leader-mode)
(setq evil-default-state 'normal)

(evil-leader/set-key "k" evil-mc-cursors-map)

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
				("b" ivy-switch-buffer))

(define-prefix-command 'my-slime-keymap)
(evil-leader/set-key "g" 'my-slime-keymap)
(my/define-keys my-slime-keymap
				("s" slime)
				("k" slime-compile-and-load-file)
				("q" slime-quit)
				("e" eval-last-sexp))

(define-prefix-command 'my-latex-render)
(evil-leader/set-key "l" 'my-latex-render)
(my/define-keys my-latex-render
				("p" preview-at-point)
				("t" texfrag-docuemnt))

(defun qutebrowser ()
  (interactive)
  (start-process-shell-command "browser" nil "qutebrowser"))

(evil-leader/set-key "u" 'qutebrowser)
(with-eval-after-load 'wdired
  (define-key wdired-mode-map (kbd "C-j") #'wdired-finish-edit))

