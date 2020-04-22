;; init-prog.el --- Initialize programming configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; General programming configurations.
;;

;;; Code:

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (when (display-graphic-p)
    (setq-default prettify-symbols-alist '(("lambda" . ?λ)
					   ("<-" . ?←)
					   ("->" . ?→)
					   ("->>" . ?↠)
					   ("=>" . ?⇒)
					   ("map" . ?↦)
					   ;; ("/=" . ?≠)
					   ;; ("!=" . ?≠)
					   ;; ("==" . ?≡)
					   ("<=" . ?≤)
					   (">=" . ?≥)
					   ("=<<" . (?= (Br . Bl) ?≪))
					   (">>=" . (?≫ (Br . Bl) ?=))
					   ("<=<" . ?↢)
					   (">=>" . ?↣)
					   ("&&" . ?∧)
					   ("||" . ?∨)
					   ("not" . ?¬)))
    (setq prettify-symbols-unprettify-at-point 'right-edge)))

(use-package fira-code-mode
  :if (display-graphic-p)
  :custom (fira-code-mode-disabled-ligatures '("x"))
  :hook prog-mode)

(use-package vimrc-mode)

;; New `conf-toml-mode' in Emacs 26
(unless (fboundp 'conf-toml-mode)
  (use-package toml-mode))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("yaml" . yaml-mode)))

(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))

(use-package highlight-indent-guides
  :ensure t
  :hook ((prog-mode . highlight-indent-guides-mode)
	 (agda2-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character))
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-hook 'agda2-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
