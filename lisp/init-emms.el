;; init-emms.el --- Initialize EMMS.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emms: A music and video player.
;;

;;; Code:

(use-package emms
  :custom
  (emms-seek-seconds 5)
  (emms-player-list '(emms-player-mpv emms-player-mplayer))
  (emms-source-file-default-directory "~/Music/")
  (emms-playlist-buffer-name "*Emms*")
  (emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
  (emms-browser-covers 'emms-browser-cache-thumbnail)
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-history-load)
  (emms-mode-line-disable))

(provide 'init-emms)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-emms.el ends here
