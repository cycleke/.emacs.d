;; init-haskell.el --- Initialize haskell configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Haskell configuration.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom)
  (require 'init-lsp))

;; Haskell Mode
(use-package haskell-mode
  :ensure nil
  :mode ("\\.hs\\'")
  :requires lsp-haskell
  :hook ((haskell-mode . lsp)
         (haskell-mode . haskell-mode-startup))

  :config
  (use-package hindent
    :config
    (add-hook 'haskell-mode-hook #'hindent-mode))
  (setq haskell-stylish-on-save t)
  (setq haskell-mode-stylish-haskell-path "brittany"))

(use-package lsp-haskell
  :defer t
  :hook ((haskell-mode) .
	 (lambda ()
	   (require 'lsp-haskell)
	   (lsp)))
  :config
  (setq lsp-haskell-process-path-hie "hie-wrapper")
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'haskell-mode-hook #'lsp-haskell-enable)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

(provide 'init-haskell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-haskell.el ends here
