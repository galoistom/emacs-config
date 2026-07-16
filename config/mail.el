;;; -*- lexical-binding: t; -*-
;;Sending Email
(require 'smtpmail)
(require 'auth-source);; probably not necessary
(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg"))

(setq user-mail-address "liug17969@gmail.com")
(setq user-full-name "galoistom")

;; 发送邮件配置
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls
      smtpmail-smtp-user "liug17969@gmail.com")

;;Debug
(setq smtpmail-debug-info t)
(setq smtpmail-debug-verb t)
