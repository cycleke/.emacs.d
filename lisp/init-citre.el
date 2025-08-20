;;; init-citre.el --- citre 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  citre 配置
;;
;;; Code:

(use-package citre
  :defer t
  :hook (after-init . (lambda () (require 'citre-config)))
  :bind
  (:map prog-mode-map
        ("C-x c j" . citre-jump)
        ("C-x c p" . citre-peek)
        ("C-x c a" . citre-ace-peek)
        ("C-x c u" . citre-update-this-tags-file))
  :custom
  (citre-ctags-default-options
   (concat "-o\12"
           "%TAGSFILE%\12"
           "-L\12%LISTFILE%\12"
           "--languages=%LANGUAGES%\12"
           "--kinds-all=*\12"
           "--kinds-C=-Dhlz\12"
           "--kinds-C++=-UZ\12"
           "--fields=*\12"
           "--fields=-ST\12"
           "--extras=*\12"
           "--tag-relative=never\12"
           "--recurse\12")))

(provide 'init-citre)
;;; init-citre.el ends here
