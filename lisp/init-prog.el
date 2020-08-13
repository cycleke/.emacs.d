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

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))

(push
 '(progn
    (use-package highlight-indent-guides
      :ensure t
      :hook (prog-mode . highlight-indent-guides-mode)
      :config
      (setq highlight-indent-guides-method 'character))
    (setq highlight-indent-guides-method 'character))
 graphic-only-plugins-setting)

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
