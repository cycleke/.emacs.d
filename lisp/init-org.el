;;; init-org.el --- Org-Mode 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;; Configuration for org-mode and related packages
;;
;;; Code:

(use-package org
  :ensure nil
  :custom
  ;; Basic behavior settings
  (org-log-done t)
  (org-log-into-drawer t)
  (org-clock-into-drawer t)
  (org-edit-timestamp-down-means-later t)
  (org-startup-indented t)
  (org-src-fontify-natively t)
  (org-html-validation-link nil)

  ;; Display settings
  (org-tags-column 80)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width 600)
  (org-outline-path-complete-in-steps nil)
  (org-pretty-entities t)

  ;; Timestamp configuration
  (org-display-custom-times t)
  (org-time-stamp-custom-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>"))

  ;; Org files and directories
  (org-directory "~/org/")
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-agenda-files '("~/org/todo.org"))

  ;; Capture templates
  (org-capture-templates
   '(("t" "待辦" entry
      (file+olp+datetree "~/org/todo.org")
      "* TODO %?\n%U\n%a"
      :clock-resume t)
     ("n" "隨記" entry
      (file+olp+datetree "~/org/notes.org" "隨記")
      "* %? :NOTE:\n%U\n%a"
      :clock-resume t)
     ("i" "巧思" entry
      (file+headline "~/org/ideas.org" "巧思")
      "* %? :IDEA:\n%U\n%a"
      :clock-resume t)))

  ;; Org-Mode TODO workflow
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(p)" "|" "DONE(d!/!)")
     (sequence "WAITING(w@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
  (org-todo-repeat-to-state "NEXT")
  (org-todo-keyword-faces
   '(("TODO" :foreground "goldenrod1" :weight bold)
     ("NEXT" :foreground "DodgerBlue1" :weight bold)
     ("IN-PROGRESS" :foreground "OrangeRed1" :weight bold)
     ("DONE" :foreground "SpringGreen2" :weight bold)
     ("WAITING" :foreground "LightSalmon1" :weight bold)
     ("HOLD" :foreground "IndianRed1" :weight bold)
     ("CANCELLED" :foreground "LavenderBlush4" :weight bold)))
  (org-todo-state-tags-triggers
   '(("CANCELLED" ("CANCELLED" . t))
     ("WAITING" ("WAITING" . t))
     (done ("WAITING"))
     ("TODO" ("WAITING") ("CANCELLED"))
     ("NEXT" ("WAITING") ("CANCELLED"))
     ("DONE" ("WAITING") ("CANCELLED"))))
  ;; Agenda settings
  (org-agenda-span 'day)
  (org-agenda-start-on-weekday nil)  ; Start agenda on current day
  (org-agenda-include-diary t)
  (org-agenda-window-setup 'current-window)

  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   :map org-mode-map
   ("C-SPC" . (lambda () (interactive) (insert "\u200B"))))
  :hook (org-mode . lu-org-mode-setup)
  :preface
  (defun lu-org-mode-setup ()
    "Setup function for org-mode with improved visual and editing experience."
    (setq-local tab-width 8
                truncate-lines nil
                line-spacing 0.1)
    (variable-pitch-mode)
    (push '("\u200B" . 8248) prettify-symbols-alist)
    (prettify-symbols-mode t)
    (visual-line-mode 1)
    (org-indent-mode 1))
  :config
  ;; Load export backends
  (require 'ox-ascii)
  (require 'ox-md)
  (require 'ox-html))

(use-package org-roam
  :ensure t
  :after org
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n c" . org-roam-capture)
   ("C-c n g" . org-roam-graph)
   (:map org-mode-map
         ("C-c n i" . org-roam-node-insert)
         ("C-c n o" . org-id-get-create)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n l" . org-roam-buffer-toggle)))
  :custom
  (org-roam-directory "~/org/roam/")
  (org-roam-completion-everywhere t)
  :config
  (org-roam-db-autosync-mode))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-hide-leading-stars nil)
  (org-superstar-leading-bullet "\u2005")
  (org-superstar-item-bullet-alist
   '((?* . ?✼) (?+ . ?✚) (?- . ?▶)))
  (org-superstar-special-todo-items t)
  (org-indent-mode-turns-on-hiding-stars nil))

(use-package toc-org
  :after org
  :hook (org-mode . toc-org-mode))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))

(use-package valign
  :disabled
  :defer t
  :hook ((org-mode markdown-mode markdown-ts-mode) . valign-mode))

(use-package mixed-pitch
  :defer t
  :hook ((org-mode markdown-mode markdown-ts-mode) . mixed-pitch-mode))

(provide 'init-org)
;;; init-org.el ends here
