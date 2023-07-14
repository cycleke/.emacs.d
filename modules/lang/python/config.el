;;; lang/python/config.el --- Python 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;; Code:

(use-package python
  :custom
  (python-shell-completion-native-enable nil)
  :config
  (when (gethash "eglot" straight--repo-cache)
    (add-hook 'python-mode-hook #'eglot-ensure)
    (when (featurep 'treesit)
      (add-hook 'python-ts-mode-hook #'eglot-ensure))))

;;; lang/python/config.el ends here
