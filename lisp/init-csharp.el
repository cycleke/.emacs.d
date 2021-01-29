;; init-csharp.el --- Initialize csharp configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; C# configuration.
;;

;;; Code:

(eval-when-compile
  (require 'init-company))

(use-package tree-sitter)
(use-package tree-sitter-langs)

(use-package csharp-mode
  :mode "\\.cs$")

(use-package shader-mode
  :mode "\\.shader$")

(use-package omnisharp
  :after company
  :hook
  (csharp-mode . (lambda ()
                   (flycheck-mode)
                   (omnisharp-mode)
                   (setq tab-width 4)
                   (setq c-basic-offset 4)))
  :init
  (add-to-list 'company-backends 'company-omnisharp))

(provide 'init-csharp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-csharp.el ends here
