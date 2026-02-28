;;; init-consult.el --- 命令補全 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;; 命令補全
;;
;;; Code:

(use-package vertico
  :hook
  (after-init . vertico-mode)
  (minibuffer-setup . vertico-repeat-save)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  ;; 不顯示不支持的 vertico 指令
  (read-extended-command-predicate
   #'command-completion-default-include-p)
  ;; minibuffer 中不顯示提示符
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :config
  ;; 補全時添加分隔提示符
  (when (< emacs-major-version 31)
    (advice-add #'completing-read-multiple :filter-args
                (lambda (args)
                  (cons (format "[CRM%s] %s"
                                (string-replace "[ \t]*" "" crm-separator)
                                (car args))
                        (cdr args))))))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package consult
  :bind
  ("C-c o" . consult-outline)
  ("C-c i" . consult-imenu)
  ("C-c I" . consult-imenu-multi)
  ("C-c r" . consult-ripgrep)

  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)

  ([remap yank-pop] . consult-yank-pop)

  ("M-g g" . consult-goto-line)
  ([remap goto-line] . consult-goto-line)

  ("C-x C-r" . consult-recent-file)
  ([remap recentf-open-files] . consult-recent-file)

  ("C-M-#" . consult-register)
  ("M-#" . consult-register-load)
  ("M-'" . consult-register-store)

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
     '("s b" . consult-buffer))))

(provide 'init-consult)
;;; init-consult.el ends here
