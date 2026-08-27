;;; -*- lexical-binding: t; -*-
(use-package time
  :config
  ;; (setq display-time-format "%d %H:%M")
  (setq display-time-format "󰥔 %d %H:%M")
  (setq display-time-default-load-average nil)
  (display-time-mode t))
;; (use-package battery
;;   :config
;;   (setq battery-mode-line-format "%q")
;;   (display-battery-mode 1))
(require 'battery)
(setq battery-mode-line-format "")
(defun my-dynamic-battery-show (&optional bat-status)
  "Render battery string from BAT-STATUS alist."
  (let ((bat-data (or bat-status
                      (and (boundp 'battery-status-function)
                           battery-status-function
                           (fboundp battery-status-function)
                           (ignore-errors (funcall battery-status-function))))))
    (if (not bat-data) ""
      (ignore-errors
        (let* ((online (string-equal (battery-format "%L" bat-data) "on-line"))
               (color (if online "#30c97b" "#F54927"))
               (bat-str (if (string-equal (battery-format "%B" bat-data) "charging")
                            (battery-format "󰂄%p%% " bat-data)
                          (battery-format "󰁹%p%% " bat-data))))
          (propertize bat-str 'face `(:weight bold :foreground ,color)))))))
(defvar my-battery-status "")
(put 'my-battery-status 'risky-local-variable t)
(defun my-update-battery (&optional status)
  "Update the STATUS of battery component."
  (setq my-battery-status (my-dynamic-battery-show status))
  (force-mode-line-update t))
(add-hook 'battery-update-functions #'my-update-battery)
(setq battery-update-interval 15)
(display-battery-mode 1)
(my-update-battery)
(defvar my-mode-line-format '("%e" mode-line-front-space
               (:propertize (""
                             mode-line-mule-info
                             mode-line-client
                             mode-line-modified
                             mode-line-remote
                             mode-line-window-dedicated)
                            display (min-width (6.0)))
               "  " mode-line-position
               mode-line-frame-identification
               mode-line-buffer-identification
               (project-mode-line project-mode-line-format)
               mode-line-format-right-align
               my-battery-status
               (vc-mode vc-mode) " "
               mode-line-modes
               mode-line-misc-info
               mode-line-end-spaces))
(setq-default mode-line-format my-mode-line-format)
(define-minor-mode my-hide-sidebar
  "Toggle modeline."
  :lighter " math-fill"
  (if my-hide-sidebar
      (setq-local mode-line-format nil)
    (setq-local mode-line-format my-mode-line-format)))

;; (use-package doom-modeline
;;   :ensure t
;;   :init
;;   (doom-modeline-mode 1)
;;   :config
;;   (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
;;   (setq doom-modeline-minor-modes nil))
