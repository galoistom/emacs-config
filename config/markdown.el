
(require 'markdown-mode)
(require 'math-preview)

(setq math-preview-command "/home/galoistom/.npm-global/bin/math-preview")
(global-hl-line-mode t)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(defun my-math-preview ()
  "在保存 Markdown 文件时更新所有公式。"
  (when (eq major-mode 'markdown-mode)
    (math-preview-all)))
(defun my-math-preview-clear-on-evil-change ()
  "如果当前缓冲区是 Markdown 模式，清除光标处的数学预览。"
  (when (eq major-mode 'markdown-mode)
    (when (fboundp 'math-preview-clear-at-point)
      (math-preview-clear-at-point))))
(add-hook 'evil-insert-state-entry-hook 'my-math-preview-clear-on-evil-change)
(add-hook 'evil-visual-state-entry-hook 'my-math-preview-clear-on-evil-change)
(add-hook 'after-save-hook 'my-math-preview)
(add-hook 'markdown-mode-hook 'my-math-preview)
(use-package texfrag
  :hook ((TeX-mode-hook . texfrag-mode)
         (markdown-mode-hook . texfrag-mode))

  :config
  ;; 所有配置都放在 :config 中，确保它们在 texfrag/preview 加载后才生效
  (setq preview-document-options '("standalone" "preview"))
  (add-to-list 'texfrag-setup-alist
             '(markdown-mode
                    ;; 起始忽略 2 行: 匹配 (```tikz 行) + (忽略行 1)
                    ;; (```tikz...)\n[^\n]*\n
                    (texfrag-block-start . "^```[[:space:]]*tikz\\(?:[[:space:]].*\\)?\n") 
                    
                    ;; 末尾只忽略 1 行: 匹配 (忽略行 N) + (``` 行)
                    ;; [^\n]*\n^```[[:space:]]*\n
                    (texfrag-block-end . "^```[[:space:]]*\n")))

  ;; 针对 preview.el 的配置需要额外的延迟处理
  (eval-after-load 'preview
    '(progn
       (add-to-list 'preview-default-preamble "\\usepackage{tikz-cd}" t)
       (add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzcd}" t)
	   (add-to-list 'preview-default-preamble
                "\\PreviewEnvironment{tikzpicture}"
                t)
	   )))
