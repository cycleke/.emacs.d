;; init-awesome-tab.el --- Initialize other packages.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; awesome-tab:
;;   Emacs package to provide out-of-the-box configuration to use tabs.
;;

;;; Code:

;; awesome-tab
(require 'awesome-tab)
(use-package awesome-tab
  :load-path "~/.emacs.d/site-lisp/awesome-tab"
  :config
  (awesome-tab-mode t)
  ;; 切换到左边的标签页
  (global-set-key (kbd "C-9") 'awesome-tab-backward-tab)
  ;; 切换到右边的标签页
  (global-set-key (kbd "C-0") 'awesome-tab-forward-tab)
  ;; 把当前标签页往右移
  (global-set-key (kbd "C-M-)") 'awesome-tab-move-current-tab-to-right)
  ;; 把当前标签页往左移
  (global-set-key (kbd "C-M-(") 'awesome-tab-move-current-tab-to-left))

(provide 'init-awesome-tab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-awesome-tab.el ends here
