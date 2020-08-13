;; init-rime.el --- Initialize emacs-rime.  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; emacs-rime: RIME ㄓ in Emacs

;;

;;; Code:

(eval-when-compile
  (require 'init-variables))

(push '(progn
         (use-package rime
           :custom
           (default-input-method "rime")
           :bind
           (:map rime-mode-map
                 ("C-`" . 'rime-send-keybinding))
           :config
           (setq rime-user-data-dir
                 (cond (sys/macp "~/Library/Rime")
                       (sys/linuxp "~/.local/share/fcitx5/rime")
                       (t "")))))
      graphic-only-plugins-setting)

(provide 'init-rime)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rime.el ends here
