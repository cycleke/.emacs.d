;;; init-data.el --- 各种結構化數據格式 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;; Code:

(use-package yaml-ts-mode
  :ensure nil
  :after (treesit)
  :hook (yaml-ts-mode . (lambda () (hungry-delete-mode -1)))
  :mode "\\.yml\\'" "\\.yaml\\'"
  :mode ".clang-tidy" ".clang-format")

(use-package nxml-mode
  :ensure nil
  :mode "\\.rss\\'"
  :mode "\\.xs\\(?:d\\|lt\\)\\'" ; xslt, xsd
  :mode "\\.p\\(?:list\\|om\\)\\'" ; plist, pom
  :config
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t))

(use-package lua-mode
  :ensure nil
  :unless (fboundp 'lua-ts-mode)
  :mode "\\.lua\\'")

(provide 'init-data)
;;; init-data.el ends here
