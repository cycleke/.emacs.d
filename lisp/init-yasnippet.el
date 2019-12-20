;; init-yasnippet.el --- Initialize yasnippet configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Yasnippet configurations.
;;

;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :bind (("M-/" . yas-expand))
  :config
  (use-package yasnippet-snippets))

(provide 'init-yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-yasnippet.el ends here
