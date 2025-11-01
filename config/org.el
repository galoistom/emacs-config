(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook ;(
    (org-mode . visual-line-mode)
    (org-mode . my/org-prettify-symbols)
    (org-mode . (lambda ()
			(electric-pair-local-mode 1)
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

;;  (add-to-list 'org-preview-latex-process-alist
;;             '(dvisvgm
;;               :programs ("pdflatex" "dvisvgm")
;;               :image-input-file "%t.pdf"
;;               :image-output-file "%o.svg"
;;               :image-command "pdflatex -halt-on-error -output-directory %o %t.tex && dvisvgm %o/%b.dvi -n -b %o/%b.svg"
;;               :image-width "(\\max (image-width \"%s\") 100)"
;;               ))
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)  ; 启用 Elisp
    (python . t)     ; 启用 Python
    (latex . t)      ; 启用 LaTeX (如果用作代码)
    (shell . t)
    ;;(ditta . t)
    ))
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
