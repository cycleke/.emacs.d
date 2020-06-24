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
  (setq awesome-tab-height 100)
  (setq awesome-tab-show-tab-index t)
  (setq awesome-tab-active-bar-width 3)
  (awesome-tab-mode t)
  ;; 切换到左边的标签页
  (global-set-key (kbd "C-9") 'awesome-tab-backward-tab)
  ;; 切换到右边的标签页
  (global-set-key (kbd "C-0") 'awesome-tab-forward-tab)
  ;; 把当前标签页往右移
  (global-set-key (kbd "C-M-)") 'awesome-tab-move-current-tab-to-right)
  ;; 把当前标签页往左移
  (global-set-key (kbd "C-M-(") 'awesome-tab-move-current-tab-to-left)
  (global-set-key (kbd "M-1") 'awesome-tab-select-visible-tab)
  (global-set-key (kbd "M-2") 'awesome-tab-select-visible-tab)
  (global-set-key (kbd "M-3") 'awesome-tab-select-visible-tab)
  (global-set-key (kbd "M-4") 'awesome-tab-select-visible-tab)
  (global-set-key (kbd "M-5") 'awesome-tab-select-visible-tab)
  (global-set-key (kbd "M-6") 'awesome-tab-select-visible-tab)
  (global-set-key (kbd "M-7") 'awesome-tab-select-visible-tab)
  (global-set-key (kbd "M-8") 'awesome-tab-select-visible-tab)
  (global-set-key (kbd "M-9") 'awesome-tab-select-visible-tab)
  (global-set-key (kbd "M-0") 'awesome-tab-select-visible-tab)
  )

(provide 'init-awesome-tab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-awesome-tab.el ends here
