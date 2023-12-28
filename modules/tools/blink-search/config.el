;;; tools/blink-search/config.el --- Blink Search -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  In the blink of an eye, the search is complete!
;;
;;; Code:

(use-package blink-search
  :straight (:host github
                   :repo "manateelazycat/blink-search"
                   :files ("*" (:exclude ".git")))
  :config
  (setq
   blink-search-search-backends
   '("Find File" "Recent File" "Grep File" "Find File" "Grep File"))

  (with-eval-after-load "meow"
    (meow-leader-define-key
     '("s d" . blink-search))
    (add-to-list 'meow-mode-state-list '(blink-search-mode . insert))))

;;; tools/blink-search/config.el ends here
