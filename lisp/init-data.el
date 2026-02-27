;;; init-data.el --- 各種結構化數據格式 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;; 結構化數據格式（XML、YAML）的支持配置
;;
;;; Code:

(use-package nxml-mode
  :ensure nil
  :mode ("\\.rss\\'"
         "\\.xs\\(?:d\\|lt\\)\\'"    ; xslt, xsd
         "\\.p\\(?:list\\|om\\)\\'") ; plist, pom
  :custom
  (nxml-slash-auto-complete-flag t)
  (nxml-auto-insert-xml-declaration-flag t))

(use-package yaml-ts-mode
  :ensure nil
  :after treesit
  :mode ("\\.ya?ml\\'"
         "\\.clang-\\(?:tidy\\|format\\)\\'")
  :preface
  (defun lu--setup-yaml-ts ()
    (hungry-delete-mode -1))
  :hook (yaml-ts-mode . lu--setup-yaml-ts))

(provide 'init-data)
;;; init-data.el ends here
