
(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  :hook
    (markdown-mode . (lambda ()
		       (add-to-list 'electric-pair-pairs '(36 . 36) t)
		       (add-to-list 'electric-pair-pairs '(42 . 42) t)))
  )
;;(require 'math-preview)

;;(setq math-preview-command "/home/galoistom/.npm-global/bin/math-preview")
;;(autoload 'markdown-mode "markdown-mode"
;;   "Major mode for editing Markdown files" t)
(defun my-math-preview-document ()
  "在保存 Markdown 文件时更新所有公式。"
  (when (eq major-mode 'markdown-mode)
    (texfrag-document)))
(defun my-math-preview-at-point ()
  "在保存 Markdown 文件时更新所有公式。"
  (when (eq major-mode 'markdown-mode)
    (preview-at-point)))
(add-hook 'after-save-hook 'my-math-preview-at-point)
(add-hook 'markdown-mode-hook 'my-math-preview-document)

(use-package texfrag
  :hook ((TeX-mode-hook . texfrag-mode)
         (markdown-mode-hook . texfrag-mode))

  :config
  (setq preview-document-options '("standalone" "preview"))
  (add-to-list 'texfrag-setup-alist
             '(markdown-mode
                    (texfrag-block-start . "^```[[:space:]]*tikz\\(?:[[:space:]].*\\)?\n")
                    
                    (texfrag-block-end . "^```[[:space:]]*\n")))

  (eval-after-load 'preview
    '(progn
       (add-to-list 'preview-default-preamble "\\usepackage{tikz-cd}" t)
       (add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzcd}" t)
	   (add-to-list 'preview-default-preamble
                "\\PreviewEnvironment{tikzpicture}"
                t)
	   )))


(setq markdown-enable-math t)
