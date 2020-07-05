;; init-markdown.el --- Initialize latex configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Markdown configuration.
;;

;;; Code:

(use-package markdown-mode+
  :ensure t
  :hook (markdown-mode . (lambda () (require 'markdown-mode+))))

(provide 'init-markdown)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markdown.el ends here
