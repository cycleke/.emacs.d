;;; tools/completion/config.el --- 文本补全工具 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  文本补全工具
;;
;;; Code:

;; 使用 Corfu 代替 Company
(use-package corfu
  :straight (:files (:defaults "extensions/*") :includes (corfu-popupinfo))
  :hook
  (emacs-startup . global-corfu-mode)
  (corfu-mode . corfu-popupinfo-mode)
  :bind
  (:map
   corfu-map
   ("<return>" . corfu-insert)
   ("<escape>" . corfu-quit)
   ([remap corfu-info-documentation] . corfu-popupinfo-toggle))
  :custom
  (completion-cycle-threshold nil)

  (corfu-cycle t)
  (corfu-auto t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-preview-current t)
  (corfu-preselect-first t)
  (corfu-echo-documentation t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)

  ;; corfu-popupinfo
  (corfu-doc-delay 0)
  (corfu-doc-max-width 70)
  (corfu-doc-max-height 20))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :commands corfu-terminal-mode
  :hook (corfu-mode . corfu-terminal-mode))

(use-package cape
  :disabled
  :init
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  :custom
  (cape-dabbrev-min-length 3)
  :config
  (defun cape-cc ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'cape-abbrev #'cape-dabbrev #'cape-file #'cape-keyword #'cape-line))))
  (add-hook 'c-mode #'cape-cc)
  (add-hook 'c++-mode #'cape-cc)
  (add-hook 'c-ts-mode #'cape-cc)
  (add-hook 'c++-ts-mode #'cape-cc))

;;; tools/completion/config.el ends here
