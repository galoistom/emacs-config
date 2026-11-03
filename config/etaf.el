;;; -*- lexical-binding: t; -*-
;;; etaf.el --- ETAF video player and Markdown preview

;; ETAF and its applications are installed as sibling checkouts; see
;; https://github.com/emacs-taf.  The roots are added to `load-path' so the
;; packages load without package.el metadata for the framework dependencies.

(defconst my/etaf-root (expand-file-name "~/Projects/emacs-taf/")
  "Directory holding the ETAF framework and application checkouts.")

(dolist (package '("ecss" "ebox" "etaf" "etaf-ui" "etaf-db"
                   "etaf-video-player" "etaf-md-preview"))
  (let ((dir (expand-file-name package my/etaf-root)))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

;;; Video player

(defun my/etaf-video-player-open-dwim ()
  "Play the video at point, or prompt for one."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (etaf-video-player-open-file (dired-get-file-for-visit))
    (call-interactively #'etaf-video-player-open-file)))

(use-package etaf-video-player
  :defer t
  :commands (etaf-video-player-open
             etaf-video-player-open-file
             etaf-video-player-open-url
             etaf-video-player-demo)
  :bind (("C-c v" . my/etaf-video-player-open-dwim)
         ("C-c V" . etaf-video-player-demo))
  :config
  ;; Render sharp image frames; "C" inside the player switches to ascii/color.
  (setq etaf-video-player-render-mode 'image
        etaf-video-player-display-fps 30
        etaf-video-player-theme-mode 'dark))

;;; Markdown preview

(defun my/etaf-md-preview-dwim ()
  "Preview the current Markdown buffer, or prompt for a file."
  (interactive)
  (if (derived-mode-p 'markdown-mode)
      (call-interactively #'etaf-md-preview)
    (call-interactively #'etaf-md-preview-open)))

(use-package etaf-md-preview
  :defer t
  :commands (etaf-md-preview
             etaf-md-preview-open)
  :bind (("C-c M" . my/etaf-md-preview-dwim))
  :config
  ;; Follow the active Emacs theme (Dracula here), with `t' inside the
  ;; preview cycling dark/light/sepia.
  (setq etaf-md-preview-theme 'auto))

;;; TeX formulas in the Markdown preview
;;
;; `etaf-md-preview' has no math node of its own.  The integration below
;; replaces every formula with a same-length marker before parsing, then
;; rewrites the parsed text runs so the marker carries the SVG image that
;; `math-preview' produces.  A paragraph that holds only a display formula is
;; centered; every other formula stays inline.  Code fences and code spans
;; are blanked before the search, so `$...$' inside code is not touched.
;; `M-x my/etaf-math-clear-cache' drops the rendered-image cache.

(declare-function math-preview--search "math-preview" (beg end))
(declare-function math-preview--submit "math-preview" (beg end string type inline))
(declare-function math-preview--extract-match "math-preview" (string))
(declare-function math-preview--overlays "math-preview" (beg end))
(declare-function etaf-ui-markdown--runs-view "etaf-ui-markdown" (inlines env base &optional extra))

(defcustom my/etaf-md-preview-math t
  "Render TeX formulas in `etaf-md-preview' with `math-preview'.
Only graphical frames are supported; in a terminal the formulas stay as
their original Markdown source."
  :type 'boolean
  :group 'etaf-md-preview)

(defvar my/etaf-math--cache (make-hash-table :test #'equal)
  "Rendered formula displays keyed by type, formula, style and font metrics.")

(defvar my/etaf-math--placeholders nil
  "Alist of placeholder strings to formula entries during one parse.")

(defun my/etaf-math--available-p ()
  "Return non-nil when formulas can be rendered in the current frame."
  (and my/etaf-md-preview-math
       (display-graphic-p)
       (require 'math-preview nil t)
       (fboundp 'math-preview--submit)
       (let ((command (if (listp math-preview-command)
                          (car math-preview-command)
                        math-preview-command)))
         (and (stringp command) (executable-find command)))))

(defun my/etaf-math--blank-region (beg end)
  "Replace the non-newline characters between BEG and END with spaces.
Each character is replaced one-for-one, so character offsets stay valid even
when the buffer holds multibyte text.  `subst-char-in-region' cannot do this:
it refuses a replacement whose byte length differs from the original."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (if (eq (char-after) ?\n)
          (forward-char 1)
        (delete-region (point) (1+ (point)))
        (insert " ")))))

(defun my/etaf-math--blank-fences ()
  "Blank fenced code blocks in the current buffer."
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*\\(`\\{3,\\}\\|~\\{3,\\}\\)[^\n]*$" nil t)
    (let* ((mark (match-string 1))
           (char (aref mark 0))
           (length (length mark))
           (beg (line-beginning-position))
           (end (point-max)))
      (forward-line 1)
      (when (re-search-forward
             (format "^[ \t]*%s\\{%d,\\}[ \t]*$"
                     (regexp-quote (char-to-string char)) length)
             nil t)
        (setq end (line-end-position)))
      (my/etaf-math--blank-region beg end)
      (goto-char end))))

(defun my/etaf-math--blank-code-spans ()
  "Blank inline code spans in the current buffer."
  (goto-char (point-min))
  (while (re-search-forward "`+" nil t)
    (let* ((beg (match-beginning 0))
           (length (- (match-end 0) beg))
           (end nil))
      (while (and (not end)
                  (re-search-forward (format "`\\{%d\\}" length) nil t))
        (when (and (not (eq (char-before (match-beginning 0)) ?`))
                   (not (eq (char-after (match-end 0)) ?`)))
          (setq end (match-end 0))))
      (when end
        (my/etaf-math--blank-region beg end)
        (goto-char end)))))

(defun my/etaf-math--preprocess (match)
  "Run `math-preview' preprocess hooks on MATCH and return it."
  (let ((type (gethash 'type match)))
    (cond ((string= type "tex")
           (run-hook-with-args 'math-preview-tex-preprocess-functions match))
          ((string= type "mathml")
           (run-hook-with-args 'math-preview-mathml-preprocess-functions match))
          ((string= type "asciimath")
           (run-hook-with-args 'math-preview-asciimath-preprocess-functions match)))
    (run-hook-with-args 'math-preview-preprocess-functions match)
    match))

(defun my/etaf-math--search (text)
  "Return math regions in TEXT as (BEG END MATCH).
Code fences and inline code are removed from the search text first, so a
formula inside code never reaches the parser.  BEG and END are zero-based
string offsets into TEXT."
  (let ((clean (with-temp-buffer
                 (insert text)
                 (my/etaf-math--blank-fences)
                 (my/etaf-math--blank-code-spans)
                 (buffer-string)))
        regions)
    (with-temp-buffer
      (insert clean)
      (dolist (region (math-preview--search (point-min) (point-max)))
        (let ((beg (1- (car region)))
              (end (1- (cdr region))))
          (when (< beg end)
            (let ((match (ignore-errors
                           (my/etaf-math--preprocess
                            (math-preview--extract-match
                             (substring text beg end))))))
              (when (and match (gethash 'string match))
                (push (list beg end match) regions)))))))
    (nreverse regions)))

(defun my/etaf-math--protect (text)
  "Return (PROTECTED . ENTRIES) for TEXT.
Each formula is replaced by a same-length marker so parser offsets and block
ranges still address the original document."
  (let ((regions (my/etaf-math--search text))
        (last 0)
        (index 0)
        pieces
        entries)
    (dolist (region regions)
      (pcase-let ((`(,beg ,end ,match) region))
        (when (>= beg last)
          (let* ((length (- end beg))
                 (marker (string (+ #xe000 index)))
                 (placeholder (concat
                               marker
                               (make-string (max 0 (1- length))
                                            #x10fffd)))
                 (entry (list :formula (string-trim (gethash 'string match))
                              :type (gethash 'type match)
                              :inline (gethash 'inline match)
                              :match (substring text beg end)
                              :placeholder placeholder
                              :index index)))
            (push (substring text last beg) pieces)
            (push placeholder pieces)
            (push (cons placeholder entry) entries)
            (setq last end
                  index (1+ index))))))
    (push (substring text last) pieces)
    (cons (apply #'concat (nreverse pieces))
          (nreverse entries))))

(defun my/etaf-math--cache-key (entry)
  "Return the cache key for formula ENTRY."
  (list (plist-get entry :type)
        (plist-get entry :formula)
        (plist-get entry :inline)
        math-preview-scale
        math-preview-margin
        (ignore-errors (frame-char-width))
        (ignore-errors (frame-char-height))))

(defun my/etaf-math--render (entry)
  "Return `math-preview' display value for formula ENTRY, or nil."
  (condition-case error
      (with-temp-buffer
        (insert "x")
        (math-preview--submit (point-min) (point-max)
                              (plist-get entry :formula)
                              (plist-get entry :type)
                              (plist-get entry :inline))
        (let ((overlay (car (math-preview--overlays (point-min) (point-max))))
              (tries 0))
          (while (and overlay
                      (not (overlay-get overlay 'display))
                      (< tries 200))
            (accept-process-output nil 0.05)
            (setq tries (1+ tries)))
          (prog1 (and overlay (overlay-get overlay 'display))
            (when overlay (delete-overlay overlay)))))
    (error
     (message "etaf-md-preview math: %s" (error-message-string error))
     nil)))

(defun my/etaf-math--display (entry)
  "Return the cached display value for formula ENTRY."
  (let ((key (my/etaf-math--cache-key entry)))
    (or (gethash key my/etaf-math--cache)
        (let ((display (my/etaf-math--render entry)))
          (when display
            (puthash key display my/etaf-math--cache))
          display))))

(defun my/etaf-math--replace-placeholders (text function)
  "Replace math placeholders in TEXT with FUNCTION's result for each entry."
  (let ((length (length text))
        (start 0)
        pieces
        found)
    (while (< start length)
      (setq found nil)
      (dolist (pair my/etaf-math--placeholders)
        (let ((index (string-search (car pair) text start)))
          (when (and index (or (null found) (< index (car found))))
            (setq found (cons index pair)))))
      (if (null found)
          (progn
            (push (substring text start) pieces)
            (setq start length))
        (let* ((index (car found))
               (pair (cdr found))
               (entry (cdr pair))
               (placeholder (car pair)))
          (when (> index start)
            (push (substring text start index) pieces))
          (push (funcall function entry) pieces)
          (setq start (+ index (length placeholder))))))
    (apply #'concat (nreverse pieces))))

(defun my/etaf-math--text-nodes (node)
  "Return NODE with math placeholders replaced by rendered images."
  (let ((text (plist-get node :text)))
    (if (not (stringp text))
        (list node)
      (list (plist-put node :text
                       (my/etaf-math--replace-placeholders
                        text
                        (lambda (entry)
                          (let ((display (my/etaf-math--display entry)))
                            (if display
                                (propertize " " 'display display)
                              (plist-get entry :match))))))))))

(defun my/etaf-math--restore-text (node)
  "Return NODE with math placeholders restored to their source text."
  (let ((text (plist-get node :text)))
    (if (not (stringp text))
        node
      (plist-put node :text
                 (my/etaf-math--replace-placeholders
                  text (lambda (entry) (plist-get entry :match)))))))

(defun my/etaf-math--restore-tree (tree)
  "Restore placeholders in TREE while preserving its nested list shape.
Table headers and rows are lists of cells, each cell a list of inline
nodes, so flattening them would break the parser's expected structure."
  (cond
   ((and (listp tree) (plist-get tree :type))
    (if (eq (plist-get tree :type) 'text)
        (my/etaf-math--restore-text tree)
      (dolist (key '(:children :items :header :rows :footnotes))
        (when (plist-member tree key)
          (plist-put tree key
                     (my/etaf-math--restore-tree (plist-get tree key)))))
      tree))
   ((listp tree)
    (mapcar #'my/etaf-math--restore-tree tree))
   (t tree)))

(defun my/etaf-math--sole-display (children)
  "Return the display-math entry when CHILDREN are only its placeholder."
  (when (and (listp children) (= (length children) 1))
    (let ((text (and (eq (plist-get (car children) :type) 'text)
                     (plist-get (car children) :text))))
      (when (and (stringp text) (not (string-empty-p text)))
        (let* ((trimmed (string-trim text))
               (pair (cl-find-if (lambda (item) (string= trimmed (car item)))
                                 my/etaf-math--placeholders)))
          (when (and pair (not (plist-get (cdr pair) :inline)))
            (cdr pair)))))))

(defun my/etaf-math--walk-list (items)
  "Walk ITEMS and return the flattened list of transformed nodes."
  (let (result)
    (dolist (item items (nreverse result))
      (dolist (node (my/etaf-math--walk item))
        (push node result)))))

(defun my/etaf-math--walk (node)
  "Return a list of nodes with math placeholders in NODE rendered."
  (cond
   ((and (listp node) (plist-get node :type))
    (let ((type (plist-get node :type)))
      (cond
       ((eq type 'text)
        (my/etaf-math--text-nodes node))
       ;; An image display property in an Ebox table cell makes the table
       ;; layout loop, so keep the TeX source there instead of rendering it.
       ((eq type 'table)
        (dolist (key '(:header :rows))
          (when (plist-member node key)
            (plist-put node key
                       (my/etaf-math--restore-tree (plist-get node key)))))
        (when (stringp (plist-get node :text))
          (plist-put node :text
                     (my/etaf-math--replace-placeholders
                      (plist-get node :text)
                      (lambda (entry) (plist-get entry :match)))))
        (list node))
       (t
        (when (and (eq type 'paragraph)
                   (my/etaf-math--sole-display (plist-get node :children)))
          (plist-put node :etaf-math-display t))
        (dolist (key '(:children :items :header :rows :footnotes))
          (when (plist-member node key)
            (plist-put node key
                       (my/etaf-math--walk-list (plist-get node key)))))
        (list node)))))
   ((and (listp node) (keywordp (car node)))
    (dolist (key '(:children :items :header :rows :footnotes))
      (when (plist-member node key)
        (plist-put node key
                   (my/etaf-math--walk-list (plist-get node key)))))
    (list node))
   ((listp node)
    (my/etaf-math--walk-list node))
   (t (list node))))

(defun my/etaf-math--parse-around (orig text flavor)
  "Parse TEXT with FLAVOR after protecting formulas for `math-preview'."
  (if (not (my/etaf-math--available-p))
      (funcall orig text flavor)
    (condition-case error
        (let* ((protected (my/etaf-math--protect text))
               (entries (cdr protected)))
          (if (null entries)
              (funcall orig text flavor)
            (let ((my/etaf-math--placeholders entries))
              (car (my/etaf-math--walk
                    (funcall orig (car protected) flavor))))))
      (error
       (if debug-on-error
           (signal (car error) (cdr error))
         (message "etaf-md-preview math setup: %s" (error-message-string error))
         (funcall orig text flavor))))))

(defun my/etaf-math--paragraph-around (orig block env)
  "Render BLOCK, centering it when it holds only a display formula."
  (if (not (plist-get block :etaf-math-display))
      (funcall orig block env)
    (etaf-node 'box
               (list :width 'stretch
                     :wrap-mode 'word
                     :text-align 'center)
               (etaf-ui-markdown--runs-view
                (plist-get block :children) env nil))))

(defun my/etaf-math-clear-cache ()
  "Clear the rendered formula cache."
  (interactive)
  (clrhash my/etaf-math--cache)
  (message "ETAF math cache cleared"))

(with-eval-after-load 'etaf-md-preview
  (advice-add 'etaf-md-preview--parse :around #'my/etaf-math--parse-around)
  (advice-add 'etaf-ui-markdown--paragraph-node :around
              #'my/etaf-math--paragraph-around))

;;; etaf.el ends here
