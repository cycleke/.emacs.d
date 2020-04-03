;; init-treemacs.el --- Initialize treemacs.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Treemacs: A tree layout file explorer.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; Require >=25.2
(when emacs/>=25.2p
  ;; A tree layout file explorer
  (use-package treemacs
    :commands (treemacs-follow-mode
               treemacs-filewatch-mode
               treemacs-fringe-indicator-mode
               treemacs-git-mode)
    :bind (([f8]        . treemacs)
           ("M-0"       . treemacs-select-window)
           ("C-x 1"     . treemacs-delete-other-windows)
           ("C-x t 1"   . treemacs-delete-other-windows)
           ("C-x t t"   . treemacs)
           ("C-x t b"   . treemacs-bookmark)
           ("C-x t C-t" . treemacs-find-file)
           ("C-x t M-t" . treemacs-find-tag)
	   ("M-RET t". awesome-fast-switch/body)
           :map treemacs-mode-map
           ([mouse-1]   . treemacs-single-click-expand-action))
    :config
    (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
          ;; treemacs-sorting                 'alphabetic-case-insensitive-desc
          treemacs-follow-after-init       t
          treemacs-is-never-other-window   t
          treemacs-silent-filewatch        t
          treemacs-silent-refresh          t
	  treemacs-no-png-images           t
          treemacs-width                   27)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (defhydra awesome-fast-switch (:hint nil)
      "
 ^^^^Fast Move             ^^^^Tab                    ^^Search            ^^Misc
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
   ^_k_^   prev group    | _C-a_^^     select first | _b_ search buffer | _C-k_   kill buffer
 _h_   _l_  switch tab   | _C-e_^^     select last  | _g_ search group  | _C-S-k_ kill others in group
   ^_j_^   next group    | _C-j_^^     ace jump     | ^^                | ^^
 ^^0 ~ 9^^ select window | _C-h_/_C-l_ move current | ^^                | ^^
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
"
      ("h" awesome-tab-backward-tab)
      ("j" awesome-tab-forward-group)
      ("k" awesome-tab-backward-group)
      ("l" awesome-tab-forward-tab)
      ("0" my-select-window)
      ("1" my-select-window)
      ("2" my-select-window)
      ("3" my-select-window)
      ("4" my-select-window)
      ("5" my-select-window)
      ("6" my-select-window)
      ("7" my-select-window)
      ("8" my-select-window)
      ("9" my-select-window)
      ("C-a" awesome-tab-select-beg-tab)
      ("C-e" awesome-tab-select-end-tab)
      ("C-j" awesome-tab-ace-jump)
      ("C-h" awesome-tab-move-current-tab-to-left)
      ("C-l" awesome-tab-move-current-tab-to-right)
      ("b" ivy-switch-buffer)
      ("g" awesome-tab-counsel-switch-group)
      ("C-k" kill-current-buffer)
      ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
      ("q" nil "quit"))

    ;; Projectile integration
    (use-package treemacs-projectile
      :after projectile
      :bind (:map projectile-command-map
		  ("h" . treemacs-projectile)))

    (use-package treemacs-magit
      :after magit
      :commands treemacs-magit--schedule-update
      :hook ((magit-post-commit
              git-commit-post-finish
              magit-post-stage
              magit-post-unstage)
             . treemacs-magit--schedule-update))))

(provide 'init-treemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treemacs.el ends here
