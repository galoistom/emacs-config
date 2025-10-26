
(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  )
;;(require 'math-preview)

;;(setq math-preview-command "/home/galoistom/.npm-global/bin/math-preview")
;;(autoload 'markdown-mode "markdown-mode"
;;   "Major mode for editing Markdown files" t)
(defun my-math-preview ()
  "在保存 Markdown 文件时更新所有公式。"
  (when (eq major-mode 'markdown-mode)
    (texfrag-document)))
(add-hook 'after-save-hook 'my-math-preview)
(add-hook 'markdown-mode-hook 'my-math-preview)
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

;;(defun my-math-preview-clear-on-evil-change ()
;;  "如果当前缓冲区是 Markdown 模式，清除光标处的数学预览。"
;;  (when (eq major-mode 'markdown-mode)
;;    (when (fboundp 'math-preview-clear-at-point)
;;      (texfrag-document))))
;;(add-hook 'evil-insert-state-entry-hook 'my-math-preview-clear-on-evil-change)
;;(add-hook 'evil-visual-state-entry-hook 'my-math-preview-clear-on-evil-change)
