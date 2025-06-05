;;; init-haskell.el --- Haskell 相关配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;; Code:

(use-package
 haskell-mode
 :mode "\\.hs\\'"
 :hook
 (haskell-mode . turn-on-haskell-doc-mode)
 (haskell-mode . hlint-refactor-mode)
 (haskell-mode . interactive-haskell-mode)
 (haskell-mode . haskell-doc-mode)
 (haskell-mode . haskell-indentation-mode))

(provide 'init-haskell)
;;; init-haskell.el ends here
