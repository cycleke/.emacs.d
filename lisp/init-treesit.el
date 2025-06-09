;;; init-treesit.el --- tree-sitter 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;; 原生 tree-sitter 配置
;;
;;; Code:

(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-font-lock-level 4)
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

(provide 'init-treesit)
;;; init-treesit.el ends here
