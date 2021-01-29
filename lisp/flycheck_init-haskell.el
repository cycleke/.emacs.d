;; init-haskell.el --- Initialize haskell configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Haskell configuration.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom)
  (require 'init-funcs)
  (require 'init-flycheck)
  (require 'init-lsp))

;; Haskell Mode
(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'")
  :bind (:map haskell-mode-map ("C-M-\\" . haskell-mode-stylish-buffer))
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'hlint-refactor-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

(use-package lsp-haskell
  :after lsp-mode
  :config
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp))

(use-package hasky-stack :ensure t)
(use-package flycheck-haskell
  :ensure t :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook #'flycheck-haskell-setup))

;; ;; Agda-mode
;; (load-file (let ((coding-system-for-read 'utf-8))
;;              (shell-command-to-string "agda-mode locate")))
;; (add-hook 'agda2-mode-hook 'prog-mode)


(provide 'init-haskell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-haskell.el ends here
