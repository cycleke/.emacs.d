;; init-emms.el --- Initialize EMMS.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emms: A music and video player.
;;

;;; Code:

(eval-when-compile
  (require 'init-variables))

(use-package emms
  :bind
  (:map leader-key
        ("m RET" . 'emms)
        ("m l" . 'emms-play-playlist)
        ("m SPC" . 'emms-pause)
        ("m n" . 'emms-next)
        ("m p" . 'emms-previous)
        ("m r" . 'emms-random)
        ("m s" . 'emms-stop)
        ("m 9" . 'emms-volume-raise)
        ("m 0" . 'emms-volume-lower))
  :custom
  (emms-seek-seconds 5)
  (emms-repeat-playlist t)
  (emms-player-list '(emms-player-mpv emms-player-mplayer))
  (emms-source-file-default-directory "~/Music/")
  (emms-playlist-buffer-name "*Music*")
  (emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
  (emms-browser-covers 'emms-browser-cache-thumbnail)
  (emms-directory (concat user-cache-directory "emms/"))
  (emms-history-file (concat emms-directory "hitsory"))
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-history-load))

(provide 'init-emms)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-emms.el ends here
