;; init-python.el --- Initialize python configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Python configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom)
  (require 'init-lsp))

;; Python Mode
;; Install:
;;   pip install pyflakes
;;   pip install autopep8
(use-package python
  :ensure nil
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))

  ;; Microsoft python-language-server support
  (use-package lsp-python-ms
    :after lsp-mode
    :hook (python-mode . (lambda ()
			   (require 'lsp-python-ms)
			   (lsp-deferred)))
    :config
    (setq lsp-python-ms-executable
	  "~/Softwares/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer"))

  ;; Live Coding in Python
  (use-package live-py-mode)

  ;; Format using YAPF
  ;; Install: pip install yapf
  (use-package yapfify
    :diminish yapf-mode
    :hook (python-mode . yapf-mode)
    :bind (:map python-mode-map
		("C-M-\\" . yapfify-buffer))))

(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
