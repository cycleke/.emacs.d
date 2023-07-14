;;; prog/lsp/config.el --- LSP 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  LSP 配置： eglot
;;
;;; Code:

;; Copied from doomemacs
(defvar +lsp--optimization-init-p nil)
(define-minor-mode +lsp-optimization-mode
  "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
  :global t
  :init-value nil
  (if (not +lsp-optimization-mode)
      (setq-default
       read-process-output-max +lsp--default-read-process-output-max
       gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
       +lsp--optimization-init-p nil)
    ;; Only apply these settings once!
    (unless +lsp--optimization-init-p
      (setq
       +lsp--default-read-process-output-max
       (default-value 'read-process-output-max)
       +lsp--default-gcmh-high-cons-threshold
       (default-value 'gcmh-high-cons-threshold))
      (setq-default read-process-output-max (* 1024 1024))
      ;; REVIEW LSP causes a lot of allocations, with or without the native JSON
      ;;        library, so we up the GC threshold to stave off GC-induced
      ;;        slowdowns/freezes. Doom uses `gcmh' to enforce its GC strategy,
      ;;        so we modify its variables rather than `gc-cons-threshold'
      ;;        directly.
      (setq-default gcmh-high-cons-threshold
                    (* 2 +lsp--default-gcmh-high-cons-threshold))
      (gcmh-set-high-threshold)
      (setq +lsp--optimization-init-p t))))

(use-package eglot
  :commands eglot eglot-ensure
  :hook (eglot-managed-mode . +lsp-optimization-mode)
  :init
  (setq
   eglot-sync-connect 1
   eglot-connect-timeout 10
   eglot-autoshutdown t
   eglot-stay-out-of '(flymake)))

(use-package consult-eglot
  :after (consult eglot)
  :bind
  (:map
   eglot-mode-map
   ([remap xref-find-apropos] . consult-eglot-symbols)))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :hook (eglot-managed-mode . flycheck-eglot-mode))

;;; prog/lsp/config.el ends here
