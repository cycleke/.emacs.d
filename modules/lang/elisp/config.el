;;; lang/elisp/config.el --- Emacs Lisp 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;; Code:

(use-package elisp-mode
  :straight (:type built-in)
  :mode ("\\.Cask\\'" . emacs-lisp-mode))

(use-package macrostep)

;;; lang/elisp/config.el ends here
