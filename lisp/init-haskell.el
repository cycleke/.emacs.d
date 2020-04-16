;; init-haskell.el --- Initialize haskell configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Haskell configuration.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; Haskell Mode
(use-package haskell-mode
  :ensure nil
  :mode ("\\.hs\\'")
  :hook ((haskell-mode . haskell-mode-startup))
  :bind (:map haskell-mode-map ("C-M-\\" . haskell-mode-stylish-buffer))
  :config
  (setq haskell-stylish-on-save t)
  (setq haskell-mode-stylish-haskell-path "brittany"))

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'dante-mode-hook
	    '(lambda () (flycheck-add-next-checker 'haskell-dante
					      '(warning . haskell-hlint))))
  :config
  (auto-save-visited-mode 1)
  (setq auto-save-visited-interval 1))

;; Agda-mode
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))


(provide 'init-haskell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-haskell.el ends here
