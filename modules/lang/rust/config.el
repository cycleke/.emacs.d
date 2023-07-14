;;; lang/rust/config.el --- Rust 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;; Code:

(use-package rust-mode
  :mode "\\.rs$"
  :init
  (when (gethash "eglot" straight--repo-cache)
    (add-hook 'rust-mode-hook #'eglot-ensure)))

(use-package rust-ts-mode
  :after treesit
  :mode "\\.proto\\'"
  :init
  (when (gethash "eglot" straight--repo-cache)
    (add-hook 'rust-ts-mode-hook #'eglot-ensure)))

(use-package flycheck-rust
  :hook
  ((rust-mode rust-ts-mode) . flycheck-mode)
  (flycheck-mode . flycheck-rust-setup))

;;; lang/rust/config.el ends here
