;;; init-eglot.el --- eglot 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  eglot 配置
;;
;;; Code:

(use-package
 eglot
 :commands eglot eglot-ensure
 :custom (eglot-sync-connect 1) (eglot-autoshutdown t)
 :init
 (with-eval-after-load 'meow
   (meow-leader-define-key
    '("l = =" . eglot-format)
    '("l = r" . eglot-format-buffer)
    '("l g d" . eglot-find-declaration)
    '("l g r" . xref-find-references)
    '("l g a" . eglot-code-actions)
    '("l r r" . eglot-rename)
    '("l ." . eldoc))))

(provide 'init-eglot)
;;; init-eglot.el ends here
