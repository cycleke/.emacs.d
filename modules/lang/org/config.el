;;; lang/org/config.el --- Org 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;; Code:

(use-package org
  :custom
  ;; 代码高亮
  (org-src-fontify-natively t))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-hide-leading-stars nil)
  (org-superstar-leading-bullet ?\s)
  (org-superstar-special-todo-items t))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

;;; lang/org/config.el ends here
