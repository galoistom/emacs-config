;;; -*- lexical-binding: t; -*-
(use-package time
  :config
  (setq display-time-format "%d %H:%M")
  ;; (setq display-time-format "󰥔 %d %H:%M")
  (setq display-time-default-load-average nil)
  (display-time-mode t))
(use-package battery
  :config
  (setq battery-mode-line-format "%q")
  (display-battery-mode 1))

;; (require 'battery)
;; (setq battery-mode-line-format nil)
;; (defun dynamic-battery-show ()
;;   "Dynamically show battery status."
;;   (if (not (fboundp battery-status-function))
;;       "foo"
;;     (let* ((bat-status (funcall battery-status-function))
;;            (online (string-equal (cdr (assoc 76 bat-status)) "on-line"))
;;            (color (if online "#30c97b" "#F54927"))
;;            (bat-str (if (string-equal (cdr (assoc 66 bat-status)) "charging")
;;                         (battery-format "󰂄%p" bat-status)
;;                         (battery-format "󰁹%p" bat-status))))
;;       (propertize bat-str 'face `(:weight bold :foreground ,color)))))
;; (display-battery-mode 1)
;; (setq-default mode-line-format
;;               '("%e" mode-line-front-space
;;                (:propertize (""
;;                              mode-line-mule-info
;;                              mode-line-client
;;                              mode-line-modified
;;                              mode-line-remote
;;                              mode-line-window-dedicated) display (min-width (6.0)))
;;                "  " mode-line-position
;;                mode-line-frame-identification
;;                mode-line-buffer-identification
;;                (project-mode-line project-mode-line-format)
;;                mode-line-format-right-align
;;                (:eval (dynamic-battery-show)) " "
;;                (vc-mode vc-mode) " "
;;                mode-line-modes
;;                mode-line-misc-info
;;                mode-line-end-spaces))

;; (dolist (hook '(eshell-mode-hook ghostel-mode-hook snake))
;;   (add-hook hook
;;             (lambda ()
;;               (setq-local mode-line-format nil))))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
  (setq doom-modeline-minor-modes nil))
