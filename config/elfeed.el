;;; -*- lexical-binding: t; -*-
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory "~/.elfeed"))

(use-package elfeed-org
  :ensure t
  :init   (setq rmh-elfeed-org-files '("~/emacs-config/elfeed.org"))
  :config
  (elfeed-org)
  (setq elfeed-feeds '()))

;; (setq elfeed-feeds
;;       '("https://iccircle.com/feed"          ; IC技术圈期刊
;;         "https://taxodium.ink/rss.xml"        ; 个人博客 Taxodium
;;         ("https://news.ycombinator.com/rss" :title "Hacker News" tech) ; 带标签的订阅
;;         ))
