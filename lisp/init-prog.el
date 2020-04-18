;; init-prog.el --- Initialize programming configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; General programming configurations.
;;

;;; Code:

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
;; (when (display-graphic-p)
;;   (use-package prog-mode
;;     :ensure nil
;;     :hook (prog-mode . prettify-symbols-mode)
;;     :init
;;     (setq-default prettify-symbols-alist '(("lambda" . ?λ)
;; 					   ("<-" . ?←)
;; 					   ("->" . ?→)
;; 					   ("->>" . ?↠)
;; 					   ("=>" . ?⇒)
;; 					   ("map" . ?↦)
;; 					   ("/=" . ?≠)
;; 					   ("!=" . ?≠)
;; 					   ("==" . ?≡)
;; 					   ("<=" . ?≤)
;; 					   (">=" . ?≥)
;; 					   ("=<<" . (?= (Br . Bl) ?≪))
;; 					   (">>=" . (?≫ (Br . Bl) ?=))
;; 					   ("<=<" . ?↢)
;; 					   (">=>" . ?↣)
;; 					   ("&&" . ?∧)
;; 					   ("||" . ?∨)
;; 					   ("not" . ?¬)))
;;     (setq prettify-symbols-unprettify-at-point 'right-edge)))

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

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
