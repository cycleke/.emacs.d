;; init-c.el --- Initialize c configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; C/C++ configuration.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom)
  (require 'init-funcs)
  (require 'init-lsp))

;; C/C++ Mode
(use-package cc-mode
  :ensure t
  :hook (c-mode-common . (lambda ()
                           (setq tab-width 2)
                           (setq c-basic-offset 2))))

(general-def
  :keymaps '(c-mode-map c++-mode-map)
  :prefix "C-c"
  "g"  'compile-with-debug
  "C-c" 'compile-without-debug)

(use-package modern-cpp-font-lock
  :ensure t
  :diminish t
  :init (modern-c++-font-lock-global-mode t))

;; C/C++/Objective-C support
(use-package ccls
  :ensure t
  :defer t
  :after lsp-mode
  :defines projectile-project-root-files-top-down-recurring
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp)))
  :custom
  (ccls-executable (executable-find "ccls"))
  (ccls-sem-highlight-method 'font-lock)
  (ccls-enable-skipped-ranges nil)
  :config
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
	        (append '("compile_commands.json" ".ccls")
		              projectile-project-root-files-top-down-recurring))))

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(provide 'init-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
