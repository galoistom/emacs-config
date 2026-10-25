;;; -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook
    (org-mode . visual-line-mode)
    (org-mode . my/org-prettify-symbols)
    (org-mode . (lambda ()
			(add-to-list 'electric-pair-pairs '(36 . 36) t)))
    (org-mode . (lambda ()
                      (modify-syntax-entry ?< "." org-mode-syntax-table)
                      (modify-syntax-entry ?> "." org-mode-syntax-table)))
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
  (require 'org-tempo)
  ;; 设置org mode中某些标签的显示字符
    (setq org-startup-folded t)
    (setq org-preview-latex-default-process 'dvisvgm)
    ;;(setq org-startup-with-latex-preview t)
    ;; (global-set-key (kbd "C-c C-p") #'org-latex-preview)
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)  ; 启用 Elisp
    (python . t)     ; 启用 Python
    (latex . t)      ; 启用 LaTeX (如果用作代码)
    (scheme . t)
    (C . t)
    (shell . t)
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
  ;; 表格边框交给 valign 自己绘制（见下面的 valign 配置）。
  ;; org-modern 会把 "|" 换成 3px 的像素竖线，并把分隔行整行缩放
  ;; （:height），这会让 valign 在 "|---+---|" 行上量错字宽，导致
  ;; 分隔行比内容行短一截、"+ " 与竖线对不齐。
  (setq org-modern-table nil)
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
  (define-key org-mode-map (kbd "C-j") ctl-x-map))
(let* ((map org-mode-map)
       (x-map (lookup-key map (kbd "C-c C-x"))))
  (define-key org-mode-map (kbd "C-j") ctl-x-map)
  (define-key map (kbd "C-c C-j") x-map)
  ;; (define-key map (kbd "C-c d") #'org-deadline)
  (define-key map (kbd "C-c C-x") #'org-goto))
(setq org-file-apps
    '(("\\.html\\'" . (lambda (file &rest _) (browse-url-generic file)))
      ("\\.pdf\\'"  . default)
      ("\\.png\\'"  . default)
      ("\\.jpg\\'"  . default)))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  :config
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n f") #'org-roam-node-find)
  (define-key org-mode-map (kbd "C-c n o") #'org-roam-ui-open)
  (define-key org-mode-map (kbd "C-c n i") #'org-roam-node-insert)
  (define-key org-mode-map (kbd "C-c n c") #'org-roam-capture)
  (define-key org-mode-map (kbd "C-c n l") #'org-roam-buffer-toggle))

(use-package websocket :ensure t)

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-ensure-on-load t))

(defvar-local my/valign-refresh-timer nil
  "Timer used to debounce table re-alignment after preview rendering.")

(defun my/valign-refresh-buffer (buffer)
  "Re-align tables in BUFFER when `valign-mode' is enabled."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (bound-and-true-p valign-mode)
                 (derived-mode-p 'org-mode)
                 (fboundp 'valign-region))
        (valign-region)))))

(defun my/valign-schedule-refresh (buffer)
  "Debounce a table re-alignment in BUFFER."
  (with-current-buffer buffer
    (when (and (bound-and-true-p valign-mode)
               (derived-mode-p 'org-mode))
      (when (timerp my/valign-refresh-timer)
        (cancel-timer my/valign-refresh-timer))
      (setq my/valign-refresh-timer
            (run-at-time 0.1 nil #'my/valign-refresh-buffer buffer)))))

(require 'math-preview)
;; ----------------------------------------------------------------------
;; 行内 `$...$' 的收紧规则
;;
;; math-preview 默认会把换行替换成空格后统一做正则匹配，因此：
;;   - 两行上各自落单的 `$' 会被跨行错误配对；
;;   - `$ 1$'、`$1 $' 这类内容首尾带空白的也会被预览。
;; 这里把单个 `$' 从通用匹配中移除，改为逐行扫描、成对校验：
;;   * 只在同一行内配对；
;;   * 单行落单的 `$' 直接忽略；
;;   * 内容为空、或以空白开头/结尾的 `$...$' 不预览；
;;   * `\$' 视为普通美元符号，不是定界符。
;; 一次配对不成立时，后一个 `$' 会作为新的起始候选继续尝试，这样
;; "价格 $5 与 $x$" 这种行里的 `$x$' 仍能正常预览。
;; ----------------------------------------------------------------------
(defun my/math-preview--escaped-p (pos)
  "Return non-nil if the character at POS is escaped by backslashes."
  (let ((n 0)
        (p (1- pos)))
    (while (and (>= p (point-min)) (eq (char-after p) ?\\))
      (setq n (1+ n)
            p (1- p)))
    (not (zerop (logand n 1)))))

(defun my/math-preview--line-dollars (beg end)
  "Return positions of unescaped dollar signs between BEG and END."
  (let (positions
        (p beg))
    (while (and p (< p end))
      (setq p (save-excursion
                (goto-char p)
                (when (search-forward "$" end t)
                  (1- (point)))))
      (when p
        (unless (my/math-preview--escaped-p p)
          (push p positions))
        (setq p (1+ p))))
    (nreverse positions)))

(defun my/math-preview--dollar-valid-p (open close)
  "Return non-nil if OPEN..CLOSE looks like inline math.
OPEN and CLOSE are the positions of the two dollar signs.  The
contents must be non-empty and must not start or end with
whitespace."
  (let ((inner (buffer-substring-no-properties (1+ open) close)))
    (and (not (string-empty-p inner))
         (not (string-match-p "\\`[[:space:]]" inner))
         (not (string-match-p "[[:space:]]\\'" inner)))))

(defun my/math-preview--dollar-regions (beg end)
  "Return same-line `$...$' regions between BEG and END."
  (let (regions)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((positions (my/math-preview--line-dollars
                          (point) (min end (line-end-position)))))
          (while positions
            (if (null (cdr positions))
                ;; 单行落单的 $，忽略。
                (setq positions nil)
              (let ((open (car positions))
                    (close (cadr positions)))
                (if (my/math-preview--dollar-valid-p open close)
                    (progn
                      (push (cons open (1+ close)) regions)
                      (setq positions (cddr positions)))
                  ;; 这一对不合法：让后一个 $ 作为新的起始候选再试。
                  (setq positions (cdr positions)))))))
        (forward-line 1)))
    (nreverse regions)))

(defun my/math-preview--regions-overlap-p (a b)
  "Return non-nil if region A overlaps region B."
  (and (< (car a) (cdr b))
       (< (car b) (cdr a))))

(defun my/math-preview--search (orig beg end)
  "Search math with ORIG between BEG and END, keeping `$...$' on a single line."
  (let* ((regions
          (let ((math-preview-tex-marks-inline
                 (seq-remove (lambda (mark)
                               (and (string= "$" (nth 0 mark))
                                    (string= "$" (nth 1 mark))))
                             math-preview-tex-marks-inline)))
            (funcall orig beg end)))
         (dollars
          (seq-remove
           (lambda (region)
             (seq-some (lambda (other)
                         (my/math-preview--regions-overlap-p region other))
                       regions))
           (my/math-preview--dollar-regions beg end))))
    (sort (append regions dollars)
          (lambda (a b) (< (car a) (car b))))))

(use-package math-preview
  :ensure t
  :custom
  ;; 设置缩放比例，根据你的屏幕高分屏调整
  (math-preview-scale 1)
  ;; 如果你希望它像 MathJax 一样处理特定的环境
  (math-preview-tex-extensions '("amsmath" "amsfnt" "braket"))
  :config
  (require 'subr-x)
  ;; 将独占一行的块级公式居中渲染。
  ;; :align-to 由显示引擎在每次重绘时求值，因此窗口大小变化后自动重新居中。
  (defun my/math-preview-center-block (orig message)
    (let ((queue-snapshot math-preview--queue))
      (funcall orig message)
      (when (and (string-prefix-p "{" message) (string-suffix-p "}" message)
                 (string-search "\"id\":" message 1))
        (let* ((msg (ignore-errors (json-read-from-string message)))
               (entry (cl-find (cdr (assoc 'id msg)) queue-snapshot
                               :key #'car :test #'eql)))
          (when (and entry
                     (string= "svg" (cdr (assoc 'type msg)))
                     (not (nth 2 entry))) ; 块级公式（非行内）
            (let ((ov (nth 1 entry)))
              (when (and (overlayp ov) (overlay-buffer ov))
                (let ((image (cadr (overlay-get ov 'display))))
                  (when (and (listp image) (eq (car image) 'image)
                             ;; 仅当公式独占一行（前后只有空白）时居中
                             (with-current-buffer (overlay-buffer ov)
                               (save-excursion
                                 (goto-char (overlay-start ov))
                                 (and (string-blank-p
                                       (buffer-substring (line-beginning-position) (point)))
                                      (progn
                                        (goto-char (overlay-end ov))
                                        (string-blank-p
                                         (buffer-substring (point) (line-end-position))))))))
                    (overlay-put ov 'before-string
                                 (propertize " " 'display
                                             `(space :align-to (+ center (-0.5 . ,image))))))))))))))
  (advice-add 'math-preview--process-input :around #'my/math-preview-center-block)
  ;; 单个 `$' 只做同行匹配，见文件上方 `my/math-preview--search'。
  (advice-add 'math-preview--search :around #'my/math-preview--search)

  ;; ------------------------------------------------------------------
  ;; 表格像素级对齐：math-preview 的图片是异步插入的 overlay，插入时不会
  ;; 触发 font-lock/jit-lock，valign 因此不会自动重新对齐，这里在每次
  ;; 图片渲染完成后手动调度一次（带防抖，避免多个公式重复对齐）。
  ;; ------------------------------------------------------------------
  (defun my/math-preview--refresh-tables (orig message)
    "After MESSAGE is rendered, re-align the table holding the preview."
    (let ((queue math-preview--queue))
      (funcall orig message)
      (when (and (string-prefix-p "{" message)
                 (string-search "\"id\":" message 1))
        (let* ((msg (ignore-errors (json-read-from-string message)))
               (entry (cl-find (cdr (assoc 'id msg)) queue
                               :key #'car :test #'eql))
               (ov (nth 1 entry)))
          (when (and (overlayp ov) (overlay-buffer ov))
            (let ((buffer (overlay-buffer ov)))
              (when (with-current-buffer buffer
                      (and (derived-mode-p 'org-mode)
                           (fboundp 'org-at-table-p)
                           (save-excursion
                             (goto-char (overlay-start ov))
                             (org-at-table-p))))
                (my/valign-schedule-refresh buffer))))))))
  (advice-add 'math-preview--process-input :around #'my/math-preview--refresh-tables)
  ;; 清除预览后单元格内容宽度会变回原始文本，同样需要重新对齐。
  (dolist (fn '(math-preview-clear-all
                math-preview-clear-region
                math-preview-clear-at-point))
    (advice-add fn :after
                (lambda (&rest _) (my/valign-schedule-refresh (current-buffer))))))

(use-package valign
  :ensure t
  :hook (org-mode . valign-mode)
  :custom
  ;; 中英文混排的表格普遍较宽，默认 4000 字符的上限很容易触发 fallback。
  (valign-max-table-size 20000)
  ;; 用 valign 自己的像素级竖线（1px，随 org-table 前景色）。
  ;; 这样内容行和 "|---+---|" 分隔行由同一套几何计算绘制，不会互相错位。
  (valign-fancy-bar t))

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
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("ctexart"  ;; 标签名，供 #+LaTeX_CLASS 调用
                 "\\documentclass[UTF8,11pt]{ctexart}" ;; 实际生成的 LaTeX 命令行
                 ("\\section{%s}" . "\\section*{%s}") ;; 对应 Org 一级标题
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


;; (unless (package-installed-p 'org-typst-preview)
;;   (package-vc-install
;;    '(org-typst-preview
;;      :url "https://github.com/roife/org-typst-preview")))
