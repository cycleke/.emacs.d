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
  (defface awesome-tray-module-rime-face
    '((((background light))
       :foreground "#008080" :bold t)
      (t
       :foreground "#00ced1" :bold t))
    "Rime ㄓ state face."
    :group 'awesome-tray)
  (defvar awesome-tray-rime-status-last-time 0)
  (defvar awesome-tray-rime-status-cache "")
  (defun awesome-tray-module-rime-info () (rime-lighter))
  (add-to-list 'awesome-tray-module-alist
               '("rime" . (awesome-tray-module-rime-info awesome-tray-module-rime-face)))

  (awesome-tray-mode 1)
  :custom
  (awesome-tray-active-modules
   '("awesome-tab" "mode-name"
     "file-path" "buffer-name" "git"
     "rime" "location" "battery" "date"))
  :config
  (add-hook 'circadian-after-load-theme-hook
            #'(lambda (_)
                (awesome-tray-mode 1))))

(provide 'init-awesome-tray)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-awesome-tray.el ends here
