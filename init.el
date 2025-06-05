;;; init.el --- Initial File -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;; 启动文件
;;
;;; Code:

(dolist (path '("site-lisp" "lisp"))
  (push (expand-file-name path user-emacs-directory) load-path))

(require 'lu-core)

(load lu-pre-custom-file 'noerror)

(require 'init-basic)
(require 'init-os)

(require 'init-edit)
(require 'init-ui)

(require 'init-corfu)
(require 'init-consult)
;; (require 'init-completion)

(require 'init-dired)
(require 'init-magit)
(require 'init-meow)

(require 'init-treesit)
(require 'init-eglot)
(require 'init-citre)

(require 'init-markdown)
(require 'init-org)

(require 'init-conf)
(require 'init-cc)
(require 'init-haskell)

(dolist (plugin '("auto-space" "protobuf-mode" "bazel"))
  (lu-byte-compile-site-lisp-if-newer plugin)
  (require (intern plugin)))

(load lu-post-custom-file)

;;; init.el ends here
