;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Customizations.
;;

;;; Code:

(eval-when-compile
  (require 'init-variables))

(defcustom my-package-archives-alist
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    `(,(cons 'official
             `(,(cons "gnu"          (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa"        (concat proto "://melpa.org/packages/"))
               ,(cons "melpa-stable" (concat proto "://stable.melpa.org/packages/"))
               ,(cons "org"          (concat proto "://orgmode.org/elpa/"))))
      ,(cons 'emacs-china
             `(,(cons "gnu"          (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa"        (concat proto "://elpa.emacs-china.org/melpa/"))
               ,(cons "melpa-stable" (concat proto "://elpa.emacs-china.org/melpa-stable/"))
               ,(cons "org"          (concat proto "://elpa.emacs-china.org/org/"))))
      ,(cons 'netease
             `(,(cons "gnu"          (concat proto "://mirrors.163.com/elpa/gnu/"))
               ,(cons "melpa"        (concat proto "://mirrors.163.com/elpa/melpa/"))
               ,(cons "melpa-stable" (concat proto "://mirrors.163.com/elpa/melpa-stable/"))
               ,(cons "org"          (concat proto "://mirrors.163.com/elpa/org/"))))
      ,(cons 'tencent
             `(,(cons "gnu"          (concat proto "://mirrors.cloud.tencent.com/elpa/gnu/"))
               ,(cons "melpa"        (concat proto "://mirrors.cloud.tencent.com/elpa/melpa/"))
               ,(cons "melpa-stable" (concat proto "://mirrors.cloud.tencent.com/elpa/melpa-stable/"))
               ,(cons "org"          (concat proto "://mirrors.cloud.tencent.com/elpa/org/"))))
      ,(cons 'tuna
             `(,(cons "gnu"          (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa"        (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
               ,(cons "melpa-stable" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/"))
               ,(cons "org"          (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/org/"))))
      ,(cons 'ustc
             `(,(cons "gnu"          (concat proto "://mirrors.ustc.edu.cn/elpa/gnu/"))
               ,(cons "melpa"        (concat proto "://mirrors.ustc.edu.cn/elpa/melpa/"))
               ,(cons "melpa-stable" (concat proto "://mirrors.ustc.edu.cn/elpa/melpa-stable/"))
               ,(cons "org"          (concat proto "://mirrors.ustc.edu.cn/elpa/org/"))))))
  "The package archives group list."
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name")))
  :group 'local)

(defcustom my-package-archives 'ustc
  "Set package archives from which to fetch."
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives
               (or (alist-get value my-package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    my-package-archives-alist))
  :group 'local)

(setq custom-file (expand-file-name "custom.el" user-cache-directory))

(if (file-exists-p custom-file)
    (load custom-file))

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
