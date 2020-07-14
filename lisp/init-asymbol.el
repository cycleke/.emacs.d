;; init-asymbol.el --- Initialize other packages.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; asymbol:
;;   fast symbol input method for latex and org mode inspired by cdlatex
;;

;;; Code:

;; using require
(require 'asymbol)
;; using use-package
(use-package asymbol
  :load-path "~/.emacs.d/site-lisp/asymbol"
  :init
  ;; add keybindings
  (asymbol-global-input-unicode-symbol-on)
  (asymbol-latex-input-symbol-on)
  (asymbol-org-input-symbol-on))

(provide 'init-asymbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-asymbol.el ends here
