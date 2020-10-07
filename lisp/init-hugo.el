;; init-hugo.el --- Initialize hugo configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Blog with Hugo configurations.
;; ox-hugo: 编写博客
;; east-hugo: 管理博客

(require 'ox)
(use-package ox-hugo
  :ensure t
  :init (require 'ox-hugo))

(use-package easy-hugo
  :init
  (setq easy-hugo-basedir "~/Blog/")
  (setq easy-hugo-postdir "content/post")
  (setq easy-hugo-url "https://cycleke.github.com")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-default-ext ".org")
  :bind ("C-c C-e" . easy-hugo))

(provide 'init-hugo)
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-hugo.el ends here
