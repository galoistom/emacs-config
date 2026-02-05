
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
;;(autoload 'markdown-mode "markdown-mode"
;;   "Major mode for editing Markdown files" t)


(use-package texfrag
  :ensure t
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
