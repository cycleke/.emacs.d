;;; prog/lsp/config.el --- 检查器配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  检查器： flycheck
;;
;;; Code:

(use-package flycheck
  :commands flycheck-list-errors flycheck-buffer
  :hook (emacs-startup . global-flycheck-mode)
  :config
  (setq
   flycheck-emacs-lisp-load-path 'inherit
   flycheck-idle-change-delay 1.0
   flycheck-display-errors-delay 0.25
   flycheck-buffer-switch-check-intermediate-buffers t))

(use-package flycheck-posframe
  :after posframe
  :config
  (setq
   flycheck-posframe-warning-prefix "! "
   flycheck-posframe-info-prefix "··· "
   flycheck-posframe-error-prefix "X "))

(use-package consult-flycheck
  :after consult
  :bind ("C-c f" . consult-flycheck))

;;; prog/lsp/config.el ends here
