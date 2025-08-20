;;; init-consult.el --- 命令补全 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  命令补全
;;
;;; Code:

(use-package vertico
  :hook (after-init . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  :bind (:map vertico-map ("DEL" . vertico-directory-delete-char))
  :config
  ;; 补全时添加分隔提示符
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string "\\`\\[.*?\\]\\*\\|\\[.*?\\]\\*\\'" "" crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; minibuffer 中不显示提示符
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; 不显示不支持的 vertico 指令
  (when (>= emacs-major-version 28)
    (setq read-extended-command-predicate #'command-completion-default-include-p))

  ;; 路径简化
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package consult
  :demand
  :bind
  ([remap goto-line] . consult-goto-line)
  ([remap imenu] . consult-imenu)
  ([remap recentf-open-files] . consult-recent-file)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap yank-pop] . consult-yank-pop)

  ("C-r" . consult-ripgrep)
  ("C-s" . consult-line)
  ("C-c i" . consult-imenu)
  ("C-c I" . consult-imenu-multi)
  ("C-x C-r" . consult-recent-file)
  ("C-x p b" . consult-project-buffer)

  :custom
  (consult-narrow-key "<")
  (consult-line-numbers-widen t)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)

  :init
  (with-eval-after-load 'meow
    (meow-leader-define-key
     '("s l" . consult-line)
     '("s L" . consult-line-multi)
     '("s f" . consult-fd)
     '("s b" . consult-buffer)
     '("s p b" . consult-project-buffer))))

(provide 'init-consult)
;;; init-consult.el ends here
