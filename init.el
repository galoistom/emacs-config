;---- regular configuration ----
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
(global-display-line-numbers-mode t)
(set-frame-parameter (selected-frame) 'background-mode 'dark)
;; 设置背景透明度 (85 是透明度百分比，范围 0-100)
;; 第一个数字是激活窗口透明度，第二个是非激活窗口透明度
;; (set-frame-parameter nil 'alpha-background 85)
;; (add-to-list 'default-frame-alist '(alpha-background . 85))

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
(defun my/create-non-existent-directories ()
  (let ((parent (file-name-directory buffer-file-name)))
    (when (and parent (not (file-exists-p parent)))
      (make-directory parent t))))

(add-hook 'find-file-not-found-functions
          #'my/create-non-existent-directories)
;; Wayland TUI clipboard bridge
(when (and (not (display-graphic-p))
           (executable-find "wl-copy")
           (executable-find "wl-paste"))
  (setq interprogram-cut-function
        (lambda (text)
          (let ((process-connection-type nil))
            (let ((proc (start-process "wl-copy" nil "wl-copy")))
              (process-send-string proc text)
              (process-send-eof proc)))))

  (setq interprogram-paste-function
        (lambda ()
          (when (executable-find "wl-paste")
            (string-trim-right
             (shell-command-to-string "wl-paste -n"))))))

(setq auto-save-default t)
(setq auto-save-timeout 5)
(setq auto-save-interval 50)
(setq display-time-format "%Y-%m-%d %H:%M")
(setq shr-use-fonts nil)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")

;---- basic packages -----
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
(load (concat custom-config-dir "my-fill.el"))
(load (concat custom-config-dir "eshell.el"))
(load (concat custom-config-dir "scheme-config.el"))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)


(use-package avy :ensure t)
(use-package eldoc-box :ensure t)
(use-package counsel :ensure t)
(use-package xdg-launcher :ensure t)
(use-package kkp :ensure t :config (global-kkp-mode 1))
(setq lsp-meson-no-auto-downloads t)
(use-package multiple-cursors :ensure t)
(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (ansi-color-apply-on-region (point-min) (point-max)))
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;gives a better status bar
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

;---- keybdings ----

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

(define-prefix-command 'my/w-prefix)
(global-set-key (kbd "C-c w") 'my/w-prefix)
(global-set-key (kbd "C-c w s") #'split-window-below)
(global-set-key (kbd "C-c w v") #'split-window-right)
(global-set-key (kbd "C-c w d") #'kill-buffer-and-window)
(global-set-key (kbd "C-c q") #'delete-other-windows)
(global-set-key (kbd "C-c w n") #'balance-windows)
(global-set-key (kbd "C-c w m") #'maximize-window)

(global-set-key (kbd "C-s")          #'flash-emacs-jump)
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

(global-set-key (kbd "C-x j")        #'goto-line)
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
(global-set-key (kbd "C-c F")        #'lsp-format-buffer)
(global-set-key (kbd "C-c m")        #'my-latex-math-auto-fill-mode)
(global-set-key (kbd "C-c p")        #'math-preview-all)
(global-set-key (kbd "C-c s")        #'my/cheatsheet)
(global-set-key (kbd "C-c f")        #'counsel-fzf)

