;;; editor/edit/config.el --- 编辑相关配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  编辑相关配置
;;
;;; Code:

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; 自动加载文件
(use-package autorevert
  :straight (:type built-in)
  :hook (emacs-startup . global-auto-revert-mode)
  :config
  (setq
   auto-revert-verbose t
   auto-revert-use-notify nil
   auto-revert-stop-on-user-input nil
   revert-without-query (list ".")))

;; 注释
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; 多光标编辑
(use-package iedit
  :bind
  ("C-'" . iedit-mode)
  ("C-x r RET" . iedit-rectangle-mode)
  (:map
   isearch-mode-map
   ("C-'" . iedit-mode-from-isearch))
  (:map
   esc-map
   ("C-'" . iedit-execute-last-modification)))

(use-package undo-fu
  :bind
  ("C-z" . undo-fu-only-undo)
  ("C-S-z" . undo-fu-only-redo)
  ("C-/" . undo-fu-only-undo)
  ("C-x u" . undo-fu-only-redo)
  :init
  (global-unset-key (kbd "C-z")))

;; 快速跳转
(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package avy
  :bind
  ("M-g f" . avy-goto-line)
  ("M-g c" . avy-goto-char)
  ("M-g w" . avy-goto-word-0))

(use-package recentf
  :ensure nil
  :hook (emacs-startup . recentf-mode)
  :custom (recentf-save-file (file-name-concat lu-cache-dir "recentf"))
  :config
  (setq recentf-max-saved-items 300
        recentf-exclude
        '(".cask"
          "COMMIT_EDITMSG\\'"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.?ido\\.last$"
          "\\.revive$"
          "/G?TAGS$"
          "/.elfeed/"
          "^/tmp/"
          "^/var/folders/.+$"
          "/persp-confs/"
          (lambda (file) (file-in-directory-p file package-user-dir)))
        recentf-keep '(file-remote-p file-readable-p)
        recentf-auto-cleanup (if (daemonp) 300))

  (add-hook 'kill-emacs-hook #'recentf-cleanup)
  (run-at-time nil 300 'recentf-save-list))

(use-package savehist
  :ensure nil
  :hook (emacs-startup . savehist-mode)
  :custom (savehist-file (file-name-concat lu-cache-dir "savehist"))
  :config
  (setq
   savehist-autosave-interval 300
   savehist-save-minibuffer-history t
   savehist-additional-variables
   '(kill-ring
     mark-ring global-mark-ring
     search-ring regexp-search-ring
     extended-command-history)
   enable-recursive-minibuffers t
   history-length 8000))

(use-package saveplace
  :hook (emacs-startup . save-place-mode)
  :custom (save-place-file (file-name-concat lu-cache-dir "saveplace")))

(use-package server
  :if (display-graphic-p)
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

(use-package tramp
  :straight (:type built-in)
  :config
  (setq
   remote-file-name-inhibit-cache 60
   tramp-completion-reread-directory-timeout 60
   tramp-verbose 1
   vc-ignore-dir-regexp
   (format "%s\\|%s\\|%s"
           vc-ignore-dir-regexp
           tramp-file-name-regexp
           "[/\\\\]node_modules")))

(use-package dtrt-indent
  :hook ((change-major-mode-after-body read-only-mode) . dtrt-indent-mode)
  :custom (dtrt-indent-verbosity 0)
  :config
  (setq
   dtrt-indent-run-after-smie t
   dtrt-indent-max-lines 2000)
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list))

(use-package helpful
  :hook (helpful-mode . visual-line-mode)
  :bind
  ([remap describe-function] . #'helpful-callable)
  ([remap describe-command]  . #'helpful-command)
  ([remap describe-variable] . #'helpful-variable)
  ([remap describe-key]      . #'helpful-key)
  ([remap describe-symbol]   . #'helpful-symbol))

(use-package smartparens
  :hook (emacs-startup . smartparens-global-mode)
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
  :config
  (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)

  (require 'smartparens-config)
  (setq
   sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   sp-highlight-wrap-tag-overlay nil
   sp-max-prefix-length 25
   sp-max-pair-length 4)

  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil))

(use-package so-long
  :hook (emacs-startup . global-so-long-mode)
  :config
  (if (fboundp 'buffer-line-statistics)
      (unless (featurep 'native-compile)
        (setq so-long-threshold 5000))
    (setq so-long-threshold 400))

  (setq so-long-minor-modes (delq 'font-lock-mode so-long-minor-modes))
  (setq so-long-minor-modes (delq 'display-line-numbers-mode so-long-minor-modes))

  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))

  (setq so-long-minor-modes
        (append so-long-minor-modes
                '(spell-fu-mode
                  eldoc-mode
                  highlight-numbers-mode
                  better-jumper-local-mode
                  ws-butler-mode
                  auto-composition-mode
                  undo-tree-mode
                  highlight-indent-guides-mode
                  hl-fill-column-mode
                  flycheck-mode
                  smartparens-mode
                  smartparens-strict-mode))))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode)
  :custom (ws-butler-keep-whitespace-before-point nil))

(use-package posframe)

;; 中英文间插入空格
(use-package pangu-spacing
  :hook (emacs-startup . global-pangu-spacing-mode)
  :config
  ;; dired-mode 不插入空格
  (dolist (hook (list 'dired-mode-hook))
    (add-hook
     hook
     (lambda ()
       (pangu-spacing-mode -1))))
  ;; org-mode markdown-mode 文本中插入空格
  (dolist (hook (list 'org-mode-hook 'markdown-mode-hook))
    (add-hook
     hook
     (lambda ()
       (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))))

(use-package highlight-indent-guides
  :if (display-graphic-p)
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom (highlight-indent-guides-method 'bitmap))

(use-package format-all
  :commands (format-all-buffer format-all-region))

;;; editor/edit/config.el ends here
