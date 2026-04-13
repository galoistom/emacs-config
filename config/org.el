;;; -*- lexical-binding: t; -*-
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook ;(
    (org-mode . visual-line-mode)
    (org-mode . my/org-prettify-symbols)
    (org-mode . (lambda ()
			(add-to-list 'electric-pair-pairs '(36 . 36) t)))
	 ;;)
  :commands (org-find-exact-headline-in-buffer org-set-tags)
  :custom-face
  ;; 设置org mode标题以及美级标题行的大小
  (org-document-title ((t (:height 1.75 :weight bold))))
  (org-level-1 ((t (:height 1.4 :weight bold))))
  (org-level-2 ((t (:height 1.35 :weight bold))))
  (org-level-3 ((t (:height 1.3 :weight bold))))
  (org-level-4 ((t (:height 1.25 :weight bold))))
  (org-level-5 ((t (:height 1.2 :weight bold))))
  (org-level-6 ((t (:height 1.15 :weight bold))))
  (org-level-7 ((t (:height 1.1 :weight bold))))
  (org-level-8 ((t (:height 1.05 :weight bold))))
  (org-level-9 ((t (:height 1.0 :weight bold))))

  ;; 设置代码块用上下边线包裹
  (org-block-begin-line ((t (:underline t :background unspecified))))
  (org-block-end-line ((t (:overline t :underline nil :background unspecified))))
  :config
  ;; 设置org mode中某些标签的显示字符
    (setq org-startup-folded t)
    (setq org-preview-latex-default-process 'dvisvgm)
<<<<<<< HEAD
    (setq org-startup-with-latex-preview t)
    (global-set-key (kbd "C-c C-p") #'org-latex-preview)
=======
    (set-face-attribute 'italic nil :family "CascadiaCodeItalic")
;    (setq org-startup-with-latex-preview t)
>>>>>>> no-evil

  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)  ; 启用 Elisp
    (python . t)     ; 启用 Python
    (latex . t)      ; 启用 LaTeX (如果用作代码)
    (scheme . t)
    (C . t)
    (shell . t)
    ;;(ditta . t)
    ))
  (setq org-confirm-babel-evaluate nil)

  (defun my/org-prettify-symbols()
    (setq prettify-symbols-alist
	  '(("[ ]" . 9744) ;; ☐
	    ("[x]" . 9745) ;; ☑
	    ("[-]" . 8863) ;; ⊟
	    ("#+begin_src" . 9998) ;; ✎
	    ("#+end_src" . 9633) ;; □
	    ("#+results:" . 9776) ;; ☰
	    ("#+attr_latex:" . "🄛")
	    ("#+attr_html:" . "🄗")
	    ("#+attr_org:" . "🄞")
	    ("#+name:" . "🄝")
	    ("#+caption:" . "🄒")
	    ("#+date:" . 128197) ;; 📅
	    ("#+author:" . 128100) ;; 💁
	    ("#+setupfile:" . 128221) ;;📝
	    ("#+email:" . 128231) ;;📧
	    ("#+startup" . 10034) ;; ✲
	    ("#+options:" . 9965) ;; ⛭
	    ("#+title:" . 10162) ;; ➲
	    ("#+subtitle:" . 11146) ;; ⮊
	    ("#+downloaded" . 8650) ;; ⇊
	    ("#+language:" . 128441) ;;🖹
	    ("#+begin_quote" . 187) ;; »
	    ("#+end_quote" . 171) ;; «
	    ("#+begin_results" . 8943) ;; ⋯
	    ("#+end_results" . 8943) ;; ⋯
    	    ("#+BEGIN_SRC" . 9998) ;; ✎
	    ("#+END_SRC" . 9633) ;; □
	    ("#+RESULTS:" . 9776) ;; ☰
	    ("#+ATTR_LATEX:" . "🄛")
	    ("#+ATTR_HTML:" . "🄗")
	    ("#+ATTR_ORG:" . "🄞")
	    ("#+NAME:" . "🄝")
	    ("#+CAPTION:" . "🄒")
	    ("#+DATE:" . 128197) ;; 📅
	    ("#+AUTHOR:" . 128100) ;; 💁
	    ("#+SETUPFILE:" . 128221) ;;📝
	    ("#+EMAIL:" . 128231) ;;📧
	    ("#+STARTUP" . 10034) ;; ✲
	    ("#+OPTIONS:" . 9965) ;; ⛭
	    ("#+TITLE:" . 10162) ;; ➲
	    ("#+SUBTITLE:" . 11146) ;; ⮊
	    ("#+DOWNLOADED" . 8650) ;; ⇊
	    ("#+LANGUAGE:" . 128441) ;;🖹
	    ("#+BEGIN_QUOTE" . 187) ;; »
	    ("#+END_QUOTE" . 171) ;; «
	    ("#+BEGIN_RESULTS" . 8943) ;; ⋯
	    ("#+END_RESULTS" . 8943) ;; ⋯
	    ))
    (setq prettify-symbols-unprettify-at-point t)
    (prettify-symbols-mode 1))

;;(plist-put! org-format-latex-options :DPI 180)
;;(plist-put! org-format-latex-options :fontsize 14)
;;(setq org-latex-preview-scale 1.5)
  :custom

  (org-fontify-whole-heading-line t)
  ;; 设置折叠符号
  (org-ellipsis " ▾")
  )
 ;; (add-hook 'after-save-hook #'(lambda ()
 ;; 			       (when (eq major-mode 'org-mode)
 ;; 				 (org-latex-preview))))

;;(use-package org-fragtog
;;  :ensure t
;;  :bind ("C-c p" . org-fragtog-mode))

;;(use-package org-latex-impatient
;;  :defer t
;;  :hook (org-mode . org-latex-impatient-mode)
;;  :init
;;  (setq org-latex-impatient-tex2svg-bin
;;        ;; location of tex2svg executable
;;        "~/node_modules/mathjax-node-cli/bin/tex2svg"))

(use-package org-modern
  :ensure t
  :hook (after-init . (lambda ()
			(setq org-modern-hide-stars 'leading)
			(global-org-modern-mode t)))
  :config
  ;; 定义各级标题行字符
  (setq org-modern-star ["◉" "○" "✸" "✳" "◈" "◇" "✿" "❀" "✜"])
  (setq-default line-spacing 0.1)
  (setq org-modern-label-border 1)
  (setq org-modern-table-vectical 2)
  (setq org-modern-table-horizontal 0)
  ;; 复选框美化
  (setq org-modern-checkbox
	'((?X . #("▢✓" 0 2 (composition ((2)))))
	  (?- . #("▢–" 0 2 (composition ((2)))))
	  (?\s . #("▢" 0 1 (composition ((1)))))))
  ;; 列表符号美化
  (setq org-modern-list
	'((?- . "•")
	  (?+ . "◦")
	  (?* . "▹")))
  ;; 代码块左边加上一条竖边线
  (setq org-modern-block-fringe t)

  ;; 属性标签使用上述定义的符号，不由 org-modern 定义
  (setq org-modern-block-name nil)
  (setq org-modern-keyword nil)
  )

(setq org-format-latex-options 
      (plist-put org-format-latex-options :use-image-magick t))

;; 确保 Org 知道使用 pdflatex (如果之前没设置的话)
;;(setq org-latex-default-packages-alist 'divsvgm)

(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)
(with-eval-after-load 'org
<<<<<<< HEAD
  (define-key org-mode-map (kbd "C-j") ctl-x-map))
=======
  (let* ((map org-mode-map)
         (x-map (lookup-key map (kbd "C-c C-x"))))
    (define-key org-mode-map (kbd "C-j") ctl-x-map)
    (define-key map (kbd "C-c C-j") x-map)
    (define-key map (kbd "C-c d") #'org-deadline)
    (define-key map (kbd "C-c C-x") #'org-goto)))
(setq org-file-apps
    '(("\\.html\\'" . (lambda (file &rest _) (browse-url-generic file)))
      ("\\.pdf\\'"  . default)
      ("\\.png\\'"  . default)
      ("\\.jpg\\'"  . default)))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
	 ("C-c n o" . org-roam-ui-open)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(use-package websocket :ensure t)

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-ensure-on-load t))

(use-package math-preview
  :ensure t
  :custom
  ;; 设置缩放比例，根据你的屏幕高分屏调整
  (math-preview-scale 1)
  ;; 如果你希望它像 MathJax 一样处理特定的环境
  (math-preview-tex-extensions '("amsmath" "amsfnt" "braket")))
(setq math-preview-command "/home/galoistom/.npm-global/bin/math-preview")
(defun my-math-preview-document ()
  "在保存 Markdown 文件时更新所有公式。"
  (when (eq major-mode 'org-mode)
    (math-preview-all)))
(defun my-eglot-format-save-setup()
  "format when save"
  (if (eglot-managed-p)
      (add-hook 'before-save-hook #'eglot-format-buffer nil t)
    (remove-hook 'before-save-hook #'eglot-format-buffer t)))
(add-hook 'after-save-hook 'my-math-preview-document)
;; (add-hook 'org-mode-hook 'my-math-preview-document)
(add-hook 'eglot-managed-mode-hook #'my-eglot-format-save-setup)

(defun my/org-export-output-bundle (orig-fun extension &optional subtreep pub-dir)
  "自动将导出文件重定向到当前目录下的 html/ 子目录。"
  (let ((pub-dir (or pub-dir "html/")))
    ;; 如果文件夹不存在，自动创建它
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir t))
    (apply orig-fun extension subtreep (list pub-dir))))

;; 仅针对 HTML 导出生效（如果你想对 PDF 或其他也生效，可以修改逻辑）
(advice-add 'org-export-output-file-name :around #'my/org-export-output-bundle)
