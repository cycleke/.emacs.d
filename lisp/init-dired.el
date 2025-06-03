;;; init-dired.el --- dired 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  Dired 配置
;;
;;; Code:

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind
  (:map dired-mode-map
        ("j" . dired-next-line)
        ("k" . dired-previous-line))
  :init
  (setq dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-clean-confirm-killing-deleted-buffers nil
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        dired-create-destination-dirs 'ask
        image-dired-dir (file-name-concat lu-cache-dir "image-dired/")
        image-dired-db-file (file-name-concat lu-data-dir "db.el")
        image-dired-gallery-dir (file-name-concat image-dired-dir "gallery/")
        image-dired-temp-image-file (file-name-concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (file-name-concat image-dired-dir "temp-rotate-image")
        image-dired-thumb-size 150)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  ;; Let OS decide how to open certain files
  (when-let* ((cmd (cond (lu-is-mac "open")
                         (lu-is-linux "xdg-open")
                         (lu-is-windows "start"))))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd)))))

(provide 'init-dired)
;;; init-dired.el ends here
