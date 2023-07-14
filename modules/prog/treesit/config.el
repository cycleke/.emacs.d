;;; prog/treesit/config.el --- tree-sitter 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;; 原生 tree-sitter 配置
;;
;;; Code:

(use-package treesit
  :straight (:type built-in)
  :if (>= emacs-major-version 29)
  :config

  (use-package treesit-auto
    :config
    (global-treesit-auto-mode)))

;;; prog/citre/config.el ends here
