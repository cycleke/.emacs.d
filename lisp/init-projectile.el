;; init-projectile.el --- Initialize projectile configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Projectile configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-variables))

;; Manage and navigate projects
(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
	            ("s-t" . projectile-find-file) ; `cmd-t' or `super-t'
	            ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :custom
  (projectile-known-projects-file
   (expand-file-name "projectile-bookmarks.eld" user-cache-directory))
  (projectile-cache-file
   (expand-file-name "projectile.cache" user-cache-directory))
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t)
  :config
  ;; (projectile-update-mode-line)         ; Update mode-line at the first time

  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  ;; Faster searching on Windows
  (when sys/win32p
    (when (or (executable-find "fd") (executable-find "rg"))
      (setq projectile-indexing-method 'alien
            projectile-enable-caching nil))

    ;; FIXME: too slow while getting submodule files on Windows
    (setq projectile-git-submodule-command nil))

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val)))

(provide 'init-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-projectile.el ends here
