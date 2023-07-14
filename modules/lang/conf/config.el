;;; lang/conf/config.el --- 各种配置文件 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;; Code:

(use-package yaml-mode
  :mode "\\.yaml$" "\\.yml$")

(use-package bazel)

(use-package nxml-mode
  :straight (:type built-in)
  :mode "\\.p\\(?:list\\|om\\)\\'" ; plist, pom
  :mode "\\.xs\\(?:d\\|lt\\)\\'"   ; xslt, xsd
  :mode "\\.rss\\'"
  :config
  (setq
   nxml-slash-auto-complete-flag t
   nxml-auto-insert-xml-declaration-flag t))

(use-package lua-mode)

(use-package protobuf-mode
  :straight (:type git :host github :repo "emacsmirror/protobuf-mode" :files ("*.el")))

(use-package protobuf-ts-mode
  :after treesit
  :mode "\\.proto\\'"
  :straight (:type git :host github :repo "emacsmirror/protobuf-ts-mode"))

;;; lang/conf/config.el ends here
