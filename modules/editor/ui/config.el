;;; editor/ui/config.el --- UI 设置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  UI 设置
;;
;;; Code:

(defvar lu-fonts nil
  "字体设置.

示例：
(setq lu-fonts
      `((default . \"Pes Mono\")
        (fixed-pitch . \"Pes Mono\")
        (variable-pitch . \"Rec Mono SemiCasual\")))
")

(defvar lu-charset-fonts nil
  "特定字符集的字体.

示例：
(setq lu-charset-fonts
      `((unicode . \"Segoe UI Emoji\")
        ((#x4e00 . #x9fff) . \"LXGW WenKai GB Screen R\")
        (symbol . \"Segoe UI Symbol\")))
")

(defvar lu-fallback-font nil
  "兜底字体.

示例：
(setq lu-fallback-font \"Segoe UI Symbol\")
")


(defun lu-font-installed-p (font-name)
  "查看 FONT-NAME 是否可用."
  (find-font (font-spec :name font-name)))

(defun lu-init-fonts (&rest _)
  "加载字体."
  (interactive)
  ;; 为不同的 face 设置字体
  (dolist (map lu-fonts)
    (when-let* ((face (car map))
                (font (cdr map)))
      (when (lu-font-installed-p font)
        (set-face-attribute face nil :font font))))
  ;; 为特定字符集设置字体
  (dolist (map lu-charset-fonts)
    (when-let* ((charset (car map))
                (font (cdr map)))
      (when (lu-font-installed-p font)
        (set-fontset-font t charset (font-spec :family font)))))
  ;; 设置兜底字体
  (when (and lu-fallback-font
             (lu-font-installed-p lu-fallback-font))
    (set-fontset-font t nil (font-spec :family lu-fallback-font) nil 'append))
  (run-hooks 'after-setting-font-hook))

(defvar lu-theme nil
  "主题设置.")

(defun lu-init-theme (&rest _)
  "加载主题."
  (interactive)
  (when (and lu-theme (not (custom-theme-enabled-p lu-theme)))
    (load-theme lu-theme t)))

(let ((hook (if (daemonp)
                'server-after-make-frame-hook
              'after-init-hook)))
  (add-hook hook #'lu-init-fonts -100)
  (add-hook hook #'lu-init-theme -90))

(setq uniquify-buffer-name-style 'forward)

(setq
 frame-title-format
 '((:eval
    (if (buffer-file-name)
        (abbreviate-file-name (buffer-file-name))
      "%b")))
 icon-title-format frame-title-format)

;; 设置缩放模式
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise nil)

;; 粘贴于光标处,而不是鼠标指针处
(setq mouse-yank-at-point t)

;; 指针设置
(blink-cursor-mode -1)
(setq blink-matching-paren nil)
(setq x-stretch-cursor nil)
(set-default 'cursor-type 'box)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; 滚动设置
(setq
 hscroll-step 1
 hscroll-margin 2
 scroll-margin 0
 scroll-conservatively 101
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01
 scroll-preserve-screen-position t
 fast-but-imprecise-scrolling t)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))
;; 鼠标滚动
(setq
 mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
 mouse-wheel-scroll-amount-horizontal 2
 mouse-wheel-progressive-speed nil)

;; 不使用对话框
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; 窗口分割
(setq
 window-divider-default-places t
 window-divider-default-bottom-width 1
 window-divider-default-right-width 1)
(setq split-width-threshold 160
      split-height-threshold nil)
(add-hook 'emacs-startup-hook #'window-divider-mode)

(use-package ediff
  :defer
  :config
  (setq
   ediff-diff-options "-w"
   ediff-split-window-function #'split-window-horizontally
   ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package hl-line
  :hook (emacs-startup . global-hl-line-mode))

(use-package paren
  :hook (emacs-startup . show-paren-mode)
  :config
  (setq
   show-paren-delay 0.1
   show-paren-highlight-openparen t
   show-paren-when-point-inside-paren t
   show-paren-when-point-in-periphery t))

(use-package whitespace
  :hook ((prog-mode conf-mode text-mode) . whitespace-mode)
  :config
  (setq
   whitespace-line-column nil
   whitespace-style
   '(face lines-tail tab-mark tabs trailing space-before-tab space-after-tab)
   whitespace-display-mappings
   '((tab-mark ?\t [?› ?\t])
     (space-mark ?\  [?·] [?.])
      (space-mark 160 [164] [95]))))

(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config
  (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(use-package display-line-numbers
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :init
  (setq-default
   display-line-numbers-width 3
   display-line-numbers-widen t))

;; 终端下忽略字体设置
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))
(unless (fboundp 'set-fontset-font)
  (fset 'set-fontset-font #'ignore))

;; 不使用 GTK+ 提示
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

(use-package ef-themes
  :init
  (setq
   ef-themes-mixed-fonts t
   ef-themes-variable-pitch-ui t))

(use-package doom-modeline
  :hook (emacs-startup . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode)
  :hook (doom-modeline-mode . column-number-mode)
  :init
  (when (>= emacs-major-version 29)
    ;; emacs 29 下 :background 不能为 nil
    (defface doom-modeline-buffer-modified
      '((t (:inherit (error bold) :background unspecified)))
      "Face used for the \\='unsaved\\=' symbol in the mode-line."
      :group 'doom-modeline-faces))

  (setq projectile-dynamic-mode-line nil)

  (setq
   doom-modeline-icon nil
   doom-modeline-bar-width 3
   doom-modeline-github nil
   doom-modeline-mu4e nil
   doom-modeline-persp-name nil
   doom-modeline-minor-modes nil
   doom-modeline-major-mode-icon nil
   doom-modeline-buffer-file-name-style 'relative-from-project
   doom-modeline-buffer-encoding 'nondefault
   doom-modeline-default-eol-type (cond (lu-is-mac 2)
                                        (lu-is-windows 1)
                                        (0)))

  (when (daemonp)
    (setq doom-modeline-icon t))
  :config
  (defvar mouse-wheel-down-event nil)
  (defvar mouse-wheel-up-event nil))

;;; editor/ui/config.el ends here
