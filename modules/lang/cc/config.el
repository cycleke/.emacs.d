;;; lang/cc/config.el --- C/C++ 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  C/C++ 配置
;;
;;; Code:

(use-package cc-mode
  :init
  (setq-default c-basic-offset 2)
  :config
  (when (gethash "eglot" straight--repo-cache)
    (add-hook 'c-mode-hook #'eglot-ensure)
    (add-hook 'c++-mode-hook #'eglot-ensure)))

(use-package c-ts-mode
  :after treesit
  :init
  (setq c-ts-mode-indent-offset 2)
  (defun cc-ts-setup ()
    (when (gethash "eglot" straight--repo-cache)
      (eglot-ensure))
    (bug-reference-prog-mode))
  :hook
  ((c-ts-mode c++-ts-mode) . cc-ts-setup))

;;; lang/cc/config.el ends here
