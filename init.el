;;; package --- Summary
;; init.el -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:
;;
;; Initialize Emacs
;;


;;; Code:

(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 200))
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(run-with-idle-timer 5 nil (lambda ()
                             (setq gc-cons-threshold gc-cons-threshold-original)
                             (setq file-name-handler-alist file-name-handler-alist-original)
                             (makunbound 'gc-cons-threshold-original)
                             (makunbound 'file-name-handler-alist-original)
                             ;; (message "gc-cons-threshold and file-name-handler-alist restored")
                             (message "自动优化完毕")))

;; Load path
;; Optimize: Force "lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "lisp" user-emacs-directory) load-path)
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)
(add-subdirs-to-load-path)

;; Constants
(require 'init-variables)

;; Customization
(require 'init-custom)

;; Functions
(require 'init-funcs)

;; Packages
(require 'init-package)

;; Preferences
(require 'init-base)
(require 'init-ui)
(require 'init-edit)
(require 'init-ivy)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-treemacs)
(require 'init-tex)
;; (require 'init-emms)
(require 'init-bongo)
(require 'init-vterm)
(require 'init-dict)
;; (require 'init-w3m)
(require 'init-awesome-tab)
;; (require 'init-awesome-tray)
(require 'init-rime)
(require 'init-asymbol)
(require 'init-markdown)
(require 'init-org)
(require 'init-hugo)
(require 'init-site-lisp)
(require 'init-keybind)

;; Programming
(require 'init-flycheck)
(require 'init-projectile)
(require 'init-lsp)
(require 'init-prog)

(require 'init-c)
(require 'init-python)
(require 'init-java)
(require 'init-web)
(require 'init-graphviz)
(require 'init-haskell)
(require 'init-graphic)

(load "server")
(unless (server-running-p) (server-start))
(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
