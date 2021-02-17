;; init-c.el --- Initialize Rust configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Rust configuration.
;;

;;; Code:

(eval-when-compile
  (require 'init-lsp))

;; Rust Mode
(use-package rust-mode
  :ensure t
  :defines lsp-rust-server
  :mode ("\\.rs\\'" . rust-mode)
  :custom
  (rust-format-on-save (executable-find "rustfmt")))

;; Cargo support
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(provide 'init-rust)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rust.el ends here
