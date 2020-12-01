;; init-rime.el --- Initialize emacs-rime.  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; emacs-rime: RIME ㄓ in Emacs

;;

;;; Code:

(eval-when-compile
  (require 'init-variables))

(push
 '(progn
    (use-package rime
      :custom
      (default-input-method "rime")
      :bind
      (:map rime-mode-map
            ("C-`" . 'rime-send-keybinding))
      :config
      (setq rime-posframe-properties
		        (list :background-color "#333333"
			            :foreground-color "#dcdccc"
			            :internal-border-width 10))
      (setq rime-user-data-dir "~/.emacs.d/pyim/rime"
            rime-show-candidate 'posframe)))
    graphic-only-plugins-setting)

(provide 'init-rime)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rime.el ends here
