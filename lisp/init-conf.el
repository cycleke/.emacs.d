;;; init-conf.el --- 各种配置文件 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;; Code:

(use-package
 yaml-mode
 :ensure nil
 :unless (fboundp 'yaml-ts-mode)
 :mode
 "\\.yaml\\'"
 "\\.yml\\'"
 :mode
 ".clang-format"
 ".clang-tidy")

(use-package
 yaml-ts-mode
 :ensure nil
 :after (treesit)
 :mode
 "\\.yaml\\'"
 "\\.yml\\'"
 :mode
 ".clang-format"
 ".clang-tidy")

(use-package
 nxml-mode
 :ensure nil
 :mode "\\.rss\\'"
 :mode "\\.xs\\(?:d\\|lt\\)\\'" ; xslt, xsd
 :mode "\\.p\\(?:list\\|om\\)\\'" ; plist, pom
 :config
 (setq
  nxml-slash-auto-complete-flag t
  nxml-auto-insert-xml-declaration-flag t))

(use-package nushell-mode :mode "\\.nu\\'")
(use-package vimrc-mode :mode "\\.vim\\'" "[._]?g?vimrc\\'")
(use-package lua-mode :ensure nil :unless (fboundp 'lua-ts-mode) :mode "\\.lua\\'")

(use-package
 sh-script
 :ensure nil
 :mode "\\.bats\\'"
 :mode "\\.\\(?:zunit\\|env\\)\\'"
 :mode "/bspwmrc\\'"
 :config
 (add-to-list
  'sh-imenu-generic-expression
  '(sh
    (nil "^\\s-*function\\s-+\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?" 1)
    (nil "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()" 1)))
 (sp-local-pair 'sh-mode "`" "`" :unless '(sp-point-before-word-p sp-point-before-same-p))
 (setq sh-indent-after-continuation 'always))

(provide 'init-conf)
;;; init-conf.el ends here
