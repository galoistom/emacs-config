
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
(require 'texfrag)
(add-hook 'markdown-mode-hook #'texfrag-mode)
;;
;;;; 告诉 texfrag-mode 识别 markdown 的代码块作为 LaTeX 环境
(add-to-list 'texfrag-setup-alist
             '(markdown-mode
               (texfrag-block-start . "^```[[:space:]]*tikz\\(?:[[:space:]].*\\)?\n")
               (texfrag-block-end . "^```[[:space:]]*\n")))
;;
;; 确保 tikz 环境被预览 (这是 Preview-LaTeX 的设置)
(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble
                "\\PreviewEnvironment{tikzpicture}"
                t))
;;(eval-after-load "preview"
;;  '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))
(setq preview-image-type 'svg)
(setq preview-image-type 'png)
;; 确保在加载 'preview' (预览系统) 之后执行
(eval-after-load 'preview
  '(progn
     ;; 1. 确保加载 tikz-cd 宏包
     (add-to-list 'preview-default-preamble 
                  "\\usepackage{tikz-cd}" t)
     
     (add-to-list 'preview-default-preamble 
                  "\\PreviewEnvironment{tikzcd}" t)))

(setq preview-document-options '("standalone" "preview"))
(add-hook 'markdown-mode-hook 'texfrag-mode)
