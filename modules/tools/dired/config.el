;;; tools/dired/config.el --- dired 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  dired 配置
;;
;;; Code:

(use-package dired
  :straight (:type built-in)
  :commands dired-jump
  :init
  (setq
   dired-dwim-target t
   dired-hide-details-hide-symlink-targets nil
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
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x
  :straight (:type built-in)
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  ;; Disable the prompt about whether I want to kill the Dired buffer for a
  ;; deleted directory. Of course I do!
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  ;; Let OS decide how to open certain files
  (when-let (cmd (cond (lu-is-mac "open")
                       (lu-is-linux "xdg-open")
                       (lu-is-windows "start")))
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

;;; tools/dired/config.el ends here
