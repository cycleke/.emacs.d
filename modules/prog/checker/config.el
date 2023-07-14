;;; prog/lsp/config.el --- 检查器配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  检查器： flyspell，flycheck，flymake
;;
;;; Code:

(use-package flyspell
  :if (or (executable-find "aspell")
          (executable-find "hunspell"))
  :commands (flyspell-mode flyspell-prog-mode)
  :config
  (cond
   ;; aspell 配置
   ((executable-find "aspell")
    (setq
     ispell-program-name "aspell"
     ispell-extra-args
     '("--sug-mode=ultra"
       "--camel-case"))

    (unless ispell-aspell-dict-dir
      (setq ispell-aspell-dict-dir
            (ispell-get-aspell-config-value "dict-dir")))
    (unless ispell-aspell-data-dir
      (setq ispell-aspell-data-dir
            (ispell-get-aspell-config-value "data-dir")))
    (unless ispell-personal-dictionary
      (setq ispell-personal-dictionary
            (expand-file-name (concat "ispell/" ispell-dictionary ".pws")
                              lu-data-dir))))
   ;; hunspell 配置
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell"))
   (t
    (setq ispell-program-name nil))))

(use-package flyspell-correct
  :after flyspell
  :commands flyspell-correct-previous
  :bind ([remap ispell-word] . flyspell-correct-at-point))

(use-package flyspell-lazy
  :after flyspell
  :config
  (setq
   flyspell-lazy-idle-seconds 1
   flyspell-lazy-window-idle-seconds 3)
  (add-hook
   'message-mode-hook
   (lambda ()
     (setq flyspell-lazy-disallow-buffers nil)))
  (flyspell-lazy-mode +1))

(use-package wucuo
  :hook
  (text-mode . wucuo-start)
  (prog-mode . wucuo-start))

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
