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
