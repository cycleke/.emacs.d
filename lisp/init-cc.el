;;; init-cc.el --- C/C++ 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  C/C++ 配置
;;
;;; Code:

(use-package c-ts-mode
  :after treesit
  :custom
  (c-ts-mode-indent-offset 2)
  (c-ts-mode-indent-style 'k&r)
  :preface
  (defun cc-ts-setup ()
    (modify-syntax-entry ?_ "w")
    (treesit-font-lock-recompute-features)
    (setq-local compile-command "clang++ -Wall -Wextra -std=c++17 "))
  :hook ((c-ts-mode c++-ts-mode) . cc-ts-setup))

(provide 'init-cc)
;;; init-cc.el ends here
