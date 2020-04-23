;; init-rime.el --- Initialize emacs-rime.  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; emacs-rime: RIME ㄓ in Emacs

;;

;;; Code:

(use-package rime
  :custom
  (default-input-method "rime")
  (rime-librime-root "~/.emacs.d/librime/dist")
  :bind
  (:map rime-mode-map
        ("C-`" . 'rime-send-keybinding))
  :config
  (setq rime-user-data-dir "~/Library/Rime"))

(provide 'init-rime)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rime.el ends here
