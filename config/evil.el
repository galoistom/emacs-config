(setq evil-want-keybinding nil)
;;(require 'evil)
(use-package evil
  :ensure t
  :config
    (evil-mode 1)
    (evil-leader/set-key
    "s" 'save-buffer      ; <Space> s -> 保存文件
    "q" 'save-buffers-kill-terminal ; <Space> q -> 保存所有并退出 Emacs
    "f" 'find-file        ; <Space> f -> 查找文件 (C-x C-f 替代)
    "e" 'dired-jump
    "h" 'dashboard-open
    "t" 'eshell
    "F" 'bookmark-jump
    )
    (evil-define-key 'normal 'global (kbd "s") 'avy-goto-char)
  )
(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "<SPC>") 
  )
;;(require 'evil-leader) 
;;(unless (package-installed-p 'evil-leader)
 ;; (package-install 'evil-leader))

(global-evil-leader-mode)

(setq evil-default-state 'normal)

(when (require 'evil-collection nil t)
  (evil-collection-init))
(global-unset-key (kbd "C-z"))

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
  (setq which-key-max-display-columns nil))

(global-set-key (kbd "C-z") 'undo)
(setq display-line-numbers 'relative)
(define-prefix-command 'my-window-map)

(evil-leader/set-key "w" 'my-window-map)

(define-key my-window-map (kbd "h") 'windmove-left)  
(define-key my-window-map (kbd "j") 'windmove-down)  
(define-key my-window-map (kbd "k") 'windmove-up)     
(define-key my-window-map (kbd "l") 'windmove-right)   
(define-key my-window-map (kbd "%") 'split-window-right) 
(define-key my-window-map (kbd "\"") 'split-window-below)  
(define-key my-window-map (kbd "x") 'delete-window)    
(define-key my-window-map (kbd "o") 'delete-other-windows) 
(define-key my-window-map (kbd "H") 'shrink-window-horizontally)
(define-key my-window-map (kbd "L") 'enlarge-window-horizontally)  
(define-key my-window-map (kbd "J") 'shrink-window)
(define-key my-window-map (kbd "K") 'enlarge-window)
(define-key my-window-map (kbd "s") 'window-swap-states) 
(define-key my-window-map (kbd "1") 'balance-windows)    

(define-prefix-command 'my-org-keymap)
(evil-leader/set-key "o" 'my-org-keymap)
(define-key my-org-keymap (kbd "v") 'org-toggle-inline-images)
(define-key my-org-keymap (kbd "c") 'org-latex-preview)
(define-key my-org-keymap (kbd "r") 'org-ctrl-c-ctrl-c)
(define-key my-org-keymap (kbd "o") 'org-open-at-point)

(define-prefix-command 'my-buffer-action)
(evil-leader/set-key "b" 'my-buffer-action)
(define-key my-buffer-action (kbd "x") 'kill-buffer)
(define-key my-buffer-action (kbd "l") 'list-buffers)
(define-key my-buffer-action (kbd "s") 'save-buffer)
(define-key my-buffer-action (kbd "b") 'switch-to-buffer)

(define-prefix-command 'my-slime-keymap)
(evil-leader/set-key "g" 'my-slime-keymap)
(define-key my-slime-keymap (kbd "s") 'slime)
(define-key my-slime-keymap (kbd "k") 'slime-compile-and-load-file)
(define-key my-slime-keymap (kbd "q") 'slime-quit)
(define-key my-slime-keymap (kbd "e") 'eval-last-sexp)

(define-prefix-command 'my-latex-render)
(evil-leader/set-key "l" 'my-latex-render)
(define-key my-latex-render (kbd "p") 'preview-at-point)
(define-key my-latex-render (kbd "t") 'texfrag-docuemnt)
