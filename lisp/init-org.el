;;; init-org.el --- Org 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;; Code:

(use-package
 org
 :ensure nil
 :custom
 ;; Basic behavior settings
 (org-log-done t) ; Keep a log when marking items as done
 (org-log-into-drawer t) ; Store logs in a drawer
 (org-clock-into-drawer t) ; Store clock data in a drawer
 (org-edit-timestamp-down-means-later t) ; Moving down means later timestamp
 (org-startup-indented t) ; Start with indentation visible
 (org-src-fontify-natively t) ; Fontify source code blocks natively
 (org-html-validation-link nil) ; Disable HTML validation link
 ;; Display settings
 (org-tags-column 80) ; Position of tags column
 (org-hide-leading-stars t) ; Hide leading stars for cleaner view
 (org-hide-emphasis-markers t) ; Hide text formatting markers
 (org-startup-with-inline-images t) ; Display inline images by default
 (org-image-actual-width 600) ; Default width for displayed images
 (org-outline-path-complete-in-steps nil) ; Show full path in header line
 (org-pretty-entities t) ; Use prettier entity display
 ;; Timestamp configuration
 (org-display-custom-times t) (org-time-stamp-custom-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>"))
 ;; Capture
 (org-capture-templates
  `(("t" "todo" entry (file "") "* NEXT %?\n%U\n" :clock-resume t)
    ("n" "note" entry (file "") "* %? :NOTE:\n%U\n%a\n" :clock-resume t)))
 ;; Org-Mode TODO
 (org-todo-keywords
  (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)") (sequence "WAITING(w@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)"))))
 (org-todo-repeat-to-state "NEXT")
 :bind (("C-c a" . org-agenda) ("C-c c" . org-capture))
 :hook (org-mode . lu-org-mode-setup)
 :preface
 (defun lu-org-mode-setup ()
   (setq-local
    tab-width 8
    truncate-lines nil)
   (variable-pitch-mode)
   (push '("​" . 8248) prettify-symbols-alist)
   (prettify-symbols-mode t))
 :config (require 'ox-ascii) (require 'ox-latex))

(use-package
 org-roam
 :ensure t
 :after org
 :bind
 (("C-c n f" . org-roam-node-find)
  ("C-c n c" . org-roam-capture) ("C-c n g" . org-roam-graph)
  (:map
   org-mode-map
   (("C-c n i" . org-roam-node-insert)
    ("C-c n o" . org-id-get-create)
    ("C-c n t" . org-roam-tag-add)
    ("C-c n a" . org-roam-alias-add)
    ("C-c n l" . org-roam-buffer-toggle))))
 :config (org-roam-db-autosync-mode))

(use-package
 org-superstar
 :hook org-mode
 :custom
 (org-hide-leading-stars nil)
 (org-superstar-item-bullet-alist '((?* . ?✼) (?+ . ?✚) (?- . ?▶)))
 (org-superstar-special-todo-items t)
 (org-indent-mode-turns-on-hiding-stars nil))

(use-package toc-org :hook org-mode)

(use-package org-appear :hook org-mode)

(use-package mixed-pitch :hook (org-mode markdown-mode markdown-ts-mode))

(use-package valign :hook (org-mode markdown-mode markdown-ts-mode))

(provide 'init-org)
;;; init-org.el ends here
