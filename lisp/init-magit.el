;;; init-magit.el --- magit 相关配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  magit 配置
;;
;;; Code:

(use-package magit
  :commands magit
  :bind ("C-x g" . magit)
  :custom
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers nil)
  (magit-revision-insert-related-refs 'mixed)
  :config
  ;; Add additional switches that seem common enough
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  (define-key transient-map [escape] #'transient-quit-one))

(provide 'init-magit)
;;; init-magit.el ends here
