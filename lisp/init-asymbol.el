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
  ;; a little customization
  (setq asymbol-help-symbol-linewidth 110
	      asymbol-help-tag-linewidth 110)

  ;; enable in org-mode and tex-mode
  (add-hook 'org-mode-hook #'asymbol-mode)
  (add-hook 'tex-mode-hook #'asymbol-mode))

(provide 'init-asymbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-asymbol.el ends here
