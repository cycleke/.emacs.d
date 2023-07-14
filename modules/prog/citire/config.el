;;; prog/treesit/config.el --- citre 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  citre 配置
;;
;;; Code:

(use-package citre
  :defer t
  :hook
  (after-init
   . (lambda ()
       (require 'citre-config)))
  :bind
  (:map
   prog-mode-map
   ("C-x c j" . citre-jump)
   ("C-x c p" . citre-peek)
   ("C-x c a" . citre-ace-peek)
   ("C-x c u" . citre-update-this-tags-file))
  :init
  (setq
   citre-enable-imenu-integration nil
   citre-enable-capf-integration nil)
  :config
  (setq
   citre-prompt-language-for-ctags-command t
   citre-auto-enable-citre-mode-modes '(prog-mode)))

;;; prog/citre/config.el ends here
