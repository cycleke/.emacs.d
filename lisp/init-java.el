;; init-java.el --- Initialize java configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Java configuration.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom)
  (require 'init-funcs)
  (require 'init-lsp))


(use-package lsp-java
  :hook (java-mode . (lambda ()
		       (require 'lsp-java)
		       (lsp-deferred))))

(provide 'init-java)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-java.el ends here
