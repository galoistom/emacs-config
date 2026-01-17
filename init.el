;--- regular configuration ---
(add-to-list 'custom-theme-load-path
             (expand-file-name "dracula" user-emacs-directory))
(load-theme 'dracula t)
(setq url-proxy-services
      '(("http"  . "127.0.0.1:7890")
        ("https" . "127.0.0.1:7890")))
(defun my/setup-fonts (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      ;; 主字体
      (set-face-attribute 'default nil
                          :family "Cascadia Code"
                          :height 140
                          :weight 'normal)

      ;; Unicode / Nerd Font fallback
      (set-fontset-font
       "fontset-default"
       'unicode
       (font-spec :family "Hack Nerd Font")
       nil
       'append))))

;; GUI 启动
(add-hook 'after-init-hook #'my/setup-fonts)

;; daemon / emacsclient 新 frame
(add-hook 'after-make-frame-functions #'my/setup-fonts)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq-default tab-width 8)
(setq standard-indent 8)
(setq inhibit-startup-screen t)
(setq select-enable-clipboard t)
(setq select-enable-primary t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode t)
(window-divider-mode t)
(electric-pair-mode 1)
(ido-mode t)
(ido-everywhere t)

(setq display-time-format "%Y-%m-%d %H:%M")

;--- basic packages ---
(require 'package)

;; 添加 MELPA 仓库源
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))
;; 加载所有已安装的包并同步仓库列表
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(setq custom-config-dir (expand-file-name "config/" user-emacs-directory))
(load (concat custom-config-dir "org.el"))
(load (concat custom-config-dir "markdown.el"))
(load (concat custom-config-dir "lsp.el"))
(load (concat custom-config-dir "my_fill.el"))
(load (concat custom-config-dir "eshell.el"))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
(global-display-line-numbers-mode t)
(set-frame-parameter (selected-frame) 'background-mode 'dark) 

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project) 
  (setq doom-modeline-minor-modes nil))

(use-package avy :ensure t)
(use-package eldoc-box :ensure t)

(setq lsp-meson-no-auto-downloads t)

(setq auto-save-default t)
(setq auto-save-timeout 5)
(setq auto-save-interval 50)

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode conf-mode yaml-mode) . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5))

(set-face-attribute 'show-paren-match nil
                    :weight 'bold
                    :background "#44475a"
                    :foreground "#ffff00")

(setq shr-use-fonts nil)
(setq browse-url-browser-function #'eww-browse-url)

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-max-description-length 40)
  (setq which-key-max-display-columns nil))

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-M-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(global-set-key (kbd "C-s") #'avy-goto-char)
(global-set-key (kbd "C-.") #'duplicate-line)
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-c i") #'mc/edit-lines)
  (global-set-key (kbd "C-c j") #'mc/unmark-previous-like-this)
  (global-set-key (kbd "C-c k") #'mc/unmark-next-like-this)
  (global-set-key (kbd "C-M-n") #'mc/mark-next-like-this)
  (global-set-key (kbd "C-M-p") #'mc/mark-previous-like-this))

(global-set-key (kbd "C-v") #'myfill)
(define-minor-mode my-cj-mode
  "Force C-j to be C-x map."
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-j") ctl-x-map)
            map))

(my-cj-mode 1)
(global-set-key (kbd "C-x C-a") 'replace-regexp)
(global-set-key (kbd "C-M-f") 'up-list)
(global-set-key (kbd "M-\"") 'shell-command)
(global-set-key (kbd "C-x c") 'compile)

(defun qutebrowser ()
  (interactive)
  (start-process-shell-command "browser" nil "qutebrowser"))

(global-set-key (kbd "C-c b") 'qutebrowser)
(global-set-key (kbd "C-c e") 'vterm)
