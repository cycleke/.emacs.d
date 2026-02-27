;;; init-haskell.el --- Haskell 相關配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;; Code:

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . haskell-doc-mode)
  (haskell-mode . haskell-indentation-mode))

(use-package hlint-refactor
  :ensure t
  :hook (haskell-mode . hlint-refactor-mode))

(provide 'init-haskell)
;;; init-haskell.el ends here
