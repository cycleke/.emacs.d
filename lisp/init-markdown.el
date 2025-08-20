;;; init-markdown.el --- Markdown 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;; Code:

(use-package markdown-mode
  :mode "\\.mdc\\'"
  :mode ("README\\.md\\'" . gfm-mode)
  :bind
  (:map markdown-mode-map
        ("C-SPC" . (lambda () (interactive) (insert "\u200B"))))
  :custom
  (markdown-enable-math t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-display-remote-images t)
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-content-type "application/xhtml+xml"))

(provide 'init-markdown)
;;; init-markdown.el ends here
