;;; tools/magit/config.el --- magit 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  magit 配置
;;
;;; Code:

(use-package magit
  :commands magit magit-file-delete
  :init
  (setq magit-auto-revert-mode nil)
  (setq transient-levels-file  (file-name-concat lu-data-dir "transient/levels")
        transient-values-file  (file-name-concat lu-data-dir "transient/values")
        transient-history-file (file-name-concat lu-data-dir "transient/history"))
  :config
  (setq
   transient-default-level 5
   magit-diff-refine-hunk t
   magit-save-repository-buffers nil
   magit-revision-insert-related-refs nil)
  (add-hook 'magit-process-mode-hook #'goto-address-mode)

  ;; Add additional switches that seem common enough
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  (define-key transient-map [escape] #'transient-quit-one))

(use-package magit-todos
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?")
  (define-key magit-todos-section-map "j" nil))

;;; tools/magit/config.el ends here
