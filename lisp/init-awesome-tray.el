;; init-awesome-tray.el --- Initialize other packages.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; awesome-tray:
;;   Hide mode-line, display necessary information at right of minibuffer.
;;

;;; Code:

;; awesome-tab
(require 'awesome-tray)
(use-package awesome-tray
  :load-path "~/.emacs.d/site-lisp/awesome-tray"
  :init
  (awesome-tray-mode 1)
  :custom
  (awesome-tray-active-modules
   '("awesome-tab" "mode-name"
     "file-path" "buffer-name" "git"
     "location" "battery" "date"))
  :config
  (add-hook 'circadian-after-load-theme-hook
            #'(lambda (_)
                (awesome-tray-mode 1))))

(provide 'init-awesome-tray)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-awesome-tray.el ends here
