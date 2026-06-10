;;; init-magit.el --- magit 相關配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;; Code:

(use-package magit
  :commands magit
  :bind ("C-x g" . magit)
  :hook (git-commit-setup . lu--git-commit-setup)
  :custom
  (git-commit-summary-max-length 72)
  (git-commit-style-convention-checks
   '(non-empty-second-line overlong-summary-line))

  (magit-refresh-verbose nil)
  (magit-log-section-commit-count 5)

  (magit-diff-refine-hunk (or lu-is-mac
                              lu-is-linux))
  (magit-revision-insert-related-refs (cond
                                       (lu-is-windows t)
                                       (t 'mixed)))
  :preface
  (defun lu--git-commit-setup ()
    (setq fill-column git-commit-summary-max-length))
  :config
  (setenv "GIT_PAGER" "cat")
  (when lu-is-windows
    (setenv "GIT_OPTIONAL_LOCKS" "0"))

  (define-key transient-map [escape] #'transient-quit-one))

(provide 'init-magit)
;;; init-magit.el ends here
