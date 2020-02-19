;; init-site-lisp.el --- Initialize other packages.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs Application FrameWork:
;;   EAF extends GNU Emacs to an entire universe of powerful GUI applications.
;;

;;; Code:

;; eaf
(require 'eaf)
(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :custom
  (eaf-find-alternate-file-in-dired t)
  :config
  (eaf-bind-key scroll_up "RET" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "DEL" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key undo_action "C-/" eaf-browser-keybinding)
  (eaf-bind-key redo_action "C-?" eaf-browser-keybinding)
  (eaf-bind-key scroll_up "M-j" eaf-browser-keybinding)
  (eaf-bind-key scroll_down "M-k" eaf-browser-keybinding)
  (eaf-bind-key scroll_up_page "M-n" eaf-browser-keybinding)
  (eaf-bind-key scroll_down_page "M-p" eaf-browser-keybinding)
  (eaf-bind-key scroll_to_begin "M->" eaf-browser-keybinding)
  (eaf-bind-key scroll_to_bottom "M-<" eaf-browser-keybinding)
  (eaf-bind-key open_link "M-h" eaf-browser-keybinding)
  (eaf-bind-key open_link_new_buffer "M-H" eaf-browser-keybinding)

  (setq eaf-grip-token "0048eacd75ec58e1df586dfc95df732ebba3258e")
  (setq eaf-proxy-type "socks5")
  (setq eaf-proxy-host "127.0.0.1")
  (setq eaf-proxy-port "1080"))

(setq browse-url-browser-function 'eaf-open-browser)
(defalias 'browse-web #'eaf-open-browser)

;; fuz.el
(require 'fuz)
(unless (require 'fuz-core nil t)
  (fuz-build-and-load-dymod))

;; snails
(require 'snails)

(provide 'init-site-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-site-lisp.el ends here
