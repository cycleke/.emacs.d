;;; init-corfu.el --- 文本补全 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  文本补全
;;
;;; Code:

(setq
 completion-ignore-case t
 completion-styles '(basic flex))

(use-package
 orderless
 :custom (orderless-component-separator "[ &]")
 :preface
 (defun lu-set-minibuffer-completion-styles ()
   (setq-local completion-styles '(substring orderless)))
 :hook (minibuffer-setup . lu-set-minibuffer-completion-styles))

;; 使用 Corfu 代替 Company
(use-package
 corfu
 :hook (after-init . global-corfu-mode) (corfu-mode . corfu-popupinfo-mode)
 :bind
 (:map
  corfu-map
  ("C-g" . corfu-quit)
  ("<escape>" . corfu-quit)
  ("<return>" . corfu-insert)

  ("M-p" . corfu-popupinfo-scroll-down)
  ("M-n" . corfu-popupinfo-scroll-up)
  ("M-d" . corfu-popupinfo-toggle)
  ("M-D" . corfu-popupinfo-location))
 :custom (completion-cycle-threshold nil)

 (corfu-cycle t) (corfu-history-mode t)

 (corfu-auto t) (corfu-auto-prefix 2) (corfu-auto-delay 0.1)

 (corfu-separator ?\s) (corfu-quit-no-match 'separator)

 (corfu-preview-current nil)

 ;; corfu-popupinfo
 (corfu-popupinfo-delay '(0.4 . 0.2)) (corfu-popupinfo-max-width 70) (corfu-popupinfo-max-height 20))

(use-package
 cape
 :init
 (add-to-list 'completion-at-point-functions #'cape-dabbrev)
 (add-to-list 'completion-at-point-functions #'cape-file)
 (add-to-list 'completion-at-point-functions #'cape-keyword)
 (add-to-list 'completion-at-point-functions #'cape-abbrev)
 (with-eval-after-load 'eglot
   (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))
 :config
 (defun cape-symbol-scapf ()
   (cape-wrap-super #'cape-abbrev #'cape-dabbrev))
 (defun cape-file-scapf ()
   (cape-wrap-super #'cape-file #'cape-history))
 (defun cape-char-scapf ()
   (cape-wrap-super #'cape-tex #'cape-sgml #'cape-rfc1345)))

(provide 'init-corfu)
;;; init-corfu.el ends here
