;; init-site-lisp.el --- Initialize other packages.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;;

;;; Code:

(eval-when-compile
  (require 'init-variables))

;; fuz.el
(use-package fuz
  :load-path "~/.emacs.d/site-lisp/fuz.el"
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod)))
(require 'fuz)

;; snails
(push
 '(progn
    (require 'snails)
    (use-package snails
      :load-path "~/.emacs.d/site-lisp/snails"
      :bind ("C-c s" . snails)))
 graphic-only-plugins-setting)

(use-package company-english-helper
  :load-path "~/.emacs.d/site-lisp/company-english-helper"
  :bind (:map leader-key ("t h" . toggle-company-english-helper)))

;; (require 'unicad)

(use-package flywrap
  :disabled
  :load-path "~/.emacs.d/site-lisp/flywrap.el"
  :hook
  ((org-mode . flywrap-mode)
   (plain-TeX-mode . flywrap-mode)
   (plain-TeX-mode . flywrap-mode)
   (markdown-mode . flywrap-mode)
   (latex-mode . flywrap-mode)
   (LaTeX-mode . flywrap-mode)
   (tex-mode . flywrap-mode)))
;; (require 'flywrap)

(provide 'init-site-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-site-lisp.el ends here
