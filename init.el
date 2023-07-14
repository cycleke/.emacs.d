;;; init.el --- -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;; Code:

;; 版本判断
(eval-and-compile
  (when (< emacs-major-version 27)
    (user-error
     (concat
      "Emacs 版本过低！！！\n"
      "当前版本 " emacs-version "，最低版本 27.1"))))

(load (expand-file-name "lu-core" user-emacs-directory))

(dolist (module
         '(
           "editor/base"
           "editor/edit"
           "editor/ui"

           "tools/magit"
           "tools/meow"
           "tools/completion"
           "tools/dired"

           "os/macos"

           "prog/treesit"
           "prog/lsp"
           ;; "prog/citire"
           "prog/checker"

           "lang/elisp"
           "lang/cc"
           "lang/rust"
           "lang/sh"
           "lang/python"
           "lang/haskell"
           "lang/conf"
           "lang/org"
           "lang/markdown"
           ))
  (load (file-name-concat lu-emacs-dir "modules/" module "config")))

(load custom-file 'noerror 'nomessage)

;;; init.el ends here
