;;; prog/lsp-bridge/config.el --- lsp-bridge 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  LSP-BRIDGE 配置
;;
;;; Code:

(use-package yasnippet)

(use-package lsp-bridge
  :straight '(lsp-bridge
              :type git
              :host github
              :repo "manateelazycat/lsp-bridge"
              :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
              :build (:not compile))

  :hook (after-init . (lambda ()
                        (yas-global-mode 1)
                        (global-lsp-bridge-mode)))
  :custom
  (acm-enable-yas nil)
  :init
  (with-eval-after-load "meow"
    (meow-leader-define-key
     '("l = =" . lsp-bridge-code-format)
     '("l r r" . lsp-bridge-rename)
     '("l g i" . lsp-bridge-find-impl)
     '("l g d" . lsp-bridge-find-def)
     '("l g D" . lsp-bridge-find-def-other-window)
     '("l g r" . lsp-bridge-find-references)
     '("l g a" . lsp-bridge-code-action)
     '("l g h" . lsp-bridge-show-documentation)
     '("l f" . lsp-bridge-diagnostic-list))

    (add-to-list 'meow-mode-state-list '(lsp-bridge-ref-mode . motion))
    (add-to-list 'meow-mode-state-list '(lsp-bridge-peek-mode . motion))
    (add-to-list 'meow-mode-state-list '(lsp-bridge-call-hierarchy-mode . motion))))

;;; prog/eglot/config.el ends here
