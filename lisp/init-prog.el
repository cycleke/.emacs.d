;; init-prog.el --- Initialize programming configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; General programming configurations.
;;

;;; Code:

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

(use-package hl-fill-column
  :ensure t
  :hook
  ((prog-mode . hl-fill-column-mode)
  (agda2-mode . hl-fill-column-mode)))

(push
 '(progn
    (use-package highlight-indent-guides
      :ensure t
      :hook ((prog-mode . highlight-indent-guides-mode)
             (agda2-mode . highlight-indent-guides-mode))
      :config
      (setq highlight-indent-guides-method 'character))
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
    (add-hook 'agda2-mode-hook 'highlight-indent-guides-mode)
    (setq highlight-indent-guides-method 'character))
 graphic-only-plugins-setting)

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
