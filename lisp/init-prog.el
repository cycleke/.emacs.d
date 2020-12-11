;; init-prog.el --- Initialize programming configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; General programming configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-variables))

(use-package vimrc-mode)

;; New `conf-toml-mode' in Emacs 26
(unless (fboundp 'conf-toml-mode)
  (use-package toml-mode))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("yaml" . yaml-mode)))

(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))

(use-package fish-mode)

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))

;; 编译运行当前文件
(use-package quickrun
  :ensure t
  :commands (quickrun)
  :bind
  (:map leader-key (("c r" . #'quickrun)
                    ("c i" . #'quickrun-shell)))
  :init (setq quickrun-timeout-seconds nil)
  (setq quickrun-focus-p nil)
  (setq quickrun-input-file-extension nil)
  :config
  (quickrun-add-command
    "python"
    '((:command . "python3")
      (:exec . "%c %s")
      (:tempfile . nil))
    :default "python")
  (quickrun-add-command
    "c++/c1z"
	  '((:command . "g++")
      (:exec    . ("%c -std=c++1z -O2 -Wall %o -o %e %s"
				           "%e %a"))
      (:remove  . ("%e")))
	  :default "c++"))

(push
 '(progn
    (use-package highlight-indent-guides
      :ensure t
      :hook (prog-mode . highlight-indent-guides-mode)
      :init
      (setq highlight-indent-guides-method 'bitmap))
    (use-package fira-code-mode
      :custom (fira-code-mode-disabled-ligatures '("[]" "x"))
      :hook prog-mode))
 graphic-only-plugins-setting)

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
