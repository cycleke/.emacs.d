;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Ivy configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))


;; 增强了搜索功能
(use-package swiper
  :bind
  (("C-s" . swiper)
   ("C-r" . counsel-rg)
   ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file))
  :config
  (setq swiper-action-recenter t)
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package counsel
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :ensure t
  :bind
  (("C-x C-r" . 'counsel-recentf)
   ("C-x d" . 'counsel-dired)
   :map swiper-map
   ("M-s" . swiper-isearch-toggle)
   ("M-%" . swiper-query-replace)

   :map isearch-mode-map
   ("M-s" . swiper-isearch-toggle)
   :map counsel-mode-map
   ([remap swiper] . counsel-grep-or-swiper)
   ([remap swiper-backward] . counsel-grep-or-swiper-backward)
   ([remap cd] . counsel-cd)
   ([remap dired] . counsel-dired)
   ([remap set-variable] . counsel-set-variable))
  :config
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
        ivy-height 10
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function nil
        ivy-initial-inputs-alist nil)
  ;; 默认的 rg 配置
  ;; (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s")
  (setq counsel-rg-base-command
        (list "rg" "-M" "240" "--with-filename" "--no-heading" "--line-number" "--color" "never" "%s" "-g" "!package-config.org" "-g" "!site-lisp"))
  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy)))

;; Enhance M-x
(use-package amx
  :init
  (setq amx-history-length 100
        amx-save-file (expand-file-name "amx-items" user-cache-directory)))

;; 强大的字符跳转工具
(use-package avy
  :ensure t
  :bind (("M-g :" . 'avy-goto-char)
         ("M-g '" . 'avy-goto-char-2)
         ("M-g \"" . 'avy-goto-char-timer)
         ("M-g f" . 'avy-goto-line)
         ("M-g w" . 'avy-goto-word-1)
         ("M-g e" . 'avy-goto-word-0)))

(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
