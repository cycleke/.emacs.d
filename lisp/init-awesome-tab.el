;; init-awesome-tab.el --- Initialize other packages.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; awesome-tab:
;;   Emacs package to provide out-of-the-box configuration to use tabs.
;;

;;; Code:

;; awesome-tab
(require 'awesome-tab)
(use-package awesome-tab
  :load-path "~/.emacs.d/site-lisp/awesome-tab"
  :init
  (defun awesome-tab-buffer-groups ()
    (list
     (cond
      ((memq major-mode '(telega-chat-mode telega-root-mode))
       "Telega")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'eshell-mode)
       "EShell")
      ((derived-mode-p 'emacs-lisp-mode)
       "Elisp")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(org-mode org-agenda-mode diary-mode))
       "OrgMode")
      (t
       (awesome-tab-get-group-name (current-buffer))))))
  :config
  (setq awesome-tab-height 100)
  (setq awesome-tab-show-tab-index t)
  (setq awesome-tab-active-bar-width 3)
  (awesome-tab-mode t)

  (general-define-key
   "C-9" 'awesome-tab-backward-tab
   "C-0" 'awesome-tab-forward-tab
   "C-M-(" 'awesome-tab-move-current-tab-to-left
   "C-M-)" 'awesome-tab-move-current-tab-to-right
   "M-1" 'awesome-tab-select-visible-tab
   "M-2" 'awesome-tab-select-visible-tab
   "M-3" 'awesome-tab-select-visible-tab
   "M-4" 'awesome-tab-select-visible-tab
   "M-5" 'awesome-tab-select-visible-tab
   "M-6" 'awesome-tab-select-visible-tab
   "M-7" 'awesome-tab-select-visible-tab
   "M-8" 'awesome-tab-select-visible-tab
   "M-9" 'awesome-tab-select-visible-tab))

(provide 'init-awesome-tab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-awesome-tab.el ends here
