;; init-graphviz.el --- Initialize graphviz-dot configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; graphviz-dot configuration.
;;

;;; Code:

(use-package graphviz-dot-mode
  :mode "\\.dot\\'"
  :bind (:map graphviz-dot-mode-map
	      ("C-M-\\" . graphviz-dot-indent-graph))
  :config
  (setq graphviz-dot-indent-width 2))

(provide 'init-graphviz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-graphviz.el ends here
