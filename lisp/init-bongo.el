;; init-bongo.el --- Initialize BONGO.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Bongo: A music and video player.
;;

;;; Code:

(eval-when-compile
  (require 'init-variables))

(use-package bongo
  :ensure t
  :hook (after-init . bongo)
  :custom
  (bongo-mode-line-icon-size 10)
  :config
  (setq bongo-header-line-mode nil)
  (setq bongo-mode-line-indicator-mode nil)
  (setq bongo-global-lastfm-mode nil)
  (defun bongo-init ()
    (interactive)
    (let ((buffer (current-buffer)))
      (bongo)
      (setq bongo-insert-whole-directory-trees "ask")
      (bongo-insert-file "~/Music")
      (bongo-insert-enqueue-region (point-min)
                                   (point-max))
      (bongo-play-random)
      (switch-to-buffer buffer)))
  :bind (:map leader-key
              ("m RET" . 'bongo-dwim)
              ("m i" . 'bongo-init)
              ("m x" . 'bongo-kill-region)
              ("m d" . 'bongo-kill-line)
              ("m _" . 'bongo-undo)
              ("m SPC" . 'bongo-pause/resume)
              ("m TAB" . 'bongo-toggle-collapsed)
              ("m h" . 'bongo-seek-backward-10)
              ("m l" . 'bongo-seek-forward-10)
              ("m a" . 'bongo-insert-enqueue)
              ("m n" . 'bongo-play-next)
              ("m p" . 'bongo-play-previous)
              ("m r" . 'bongo-play-random)
              ("m s" . 'bongo-sprinkle)))

(provide 'init-bongo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-bongo.el ends here
