;;; init-edit.el --- 编辑相关配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  编辑相关配置
;;
;;; Code:

;; 文件末尾添加空白行
(setq require-final-newline t)

;; 设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插
(setq-default sentence-end-double-space nil)
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

;; 缩进和 Tab 设置
(setq-default indent-tabs-mode nil
              tab-width 4
              tab-always-indent nil)
(setq tabify-regexp "^\\t* [ \\t]+")

;; 保持补全的原始大小写
(setq dabbrev-case-replace nil)

;; elisp 添加告警
(use-package flymake
  :ensure nil
  :hook (emacs-lisp-mode eglot-managed-mode)
  :init
  (with-eval-after-load 'meow
    (with-eval-after-load 'consult
      (meow-leader-define-key '("f" . consult-flymake)))))

;; 自动加载文件
(global-auto-revert-mode)
(bind-key "C-M-g" #'revert-buffer)
(setq auto-revert-verbose t
      auto-revert-use-notify nil
      auto-revert-stop-on-user-input nil
      revert-without-query (list "."))

;; 注释
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; 删除空格
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode))

;; 多光标编辑
(use-package iedit
  :bind ("C-;" . iedit-mode))

(use-package smartparens
  :diminish
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config))

;; 撤销
(use-package vundo
  :bind ("C-x u" . vundo))

;; 快速跳转
(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom (aw-scope 'frame))

;; 保存最近打开文件
(recentf-mode 1)
(add-hook 'kill-emacs-hook #'recentf-cleanup)
(setq recentf-auto-cleanup 600
      recentf-max-saved-items 1000
      recentf-save-file (file-name-concat lu-cache-dir "recentf")
      recentf-exclude
      '("recentf"
        "/elpa/"
        ".*?autoloads.el$"
        "COMMIT_EDITMSG\\'"
        "/.git/"
        ".gitignore"
        "^/tmp/"
        "/AppData/Local/Temp/"
        "^/var/folders/.+$"
        "/TAGS$"
        "/.TAGS$"
        "/tags$"
        "/.tags$"
        "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)\\'"
        (lambda (file) (file-in-directory-p file package-user-dir)))
      recentf-keep '(file-remote-p file-readable-p))

;; 保存历史记录
(savehist-mode 1)
(setq savehist-autosave-interval 300
      savehist-save-minibuffer-history t
      savehist-file (file-name-concat lu-cache-dir "savehist")
      savehist-additional-variables
      '(kill-ring mark-ring global-mark-ring search-ring
                  regexp-search-ring extended-command-history)
      enable-recursive-minibuffers t
      history-length 8000)

;; 保存光标位置
(save-place-mode 1)
(setq save-place-file (file-name-concat lu-cache-dir "saveplace"))

(use-package tramp
  :defer 5
  :custom
  (remote-file-name-inhibit-cache 60)
  (tramp-default-method "ssh")
  (tramp-use-connection-share nil)
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                vc-ignore-dir-regexp
                                tramp-file-name-regexp)))

(use-package dtrt-indent
  :diminish
  :ensure t
  :hook (change-major-mode-after-body read-only-mode)
  :custom
  (dtrt-indent-verbosity 0)
  (dtrt-indent-run-after-smie t)
  (dtrt-indent-max-lines 2000)
  :config
  (add-to-list 'dtrt-indent-hook-generic-mapping-list '(t tab-width)))

(use-package helpful
  :hook (helpful-mode . visual-line-mode)
  :bind
  ([remap describe-function] . #'helpful-callable)
  ([remap describe-command] . #'helpful-command)
  ([remap describe-variable] . #'helpful-variable)
  ([remap describe-key] . #'helpful-key)
  ([remap describe-symbol] . #'helpful-symbol))

(use-package so-long
  :hook (after-init . global-so-long-mode)
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
                '(eldoc-mode
                  highlight-numbers-mode
                  ws-butler-mode
                  auto-composition-mode
                  vundo-mode
                  smartparens-mode
                  smartparens-strict-mode))))

(use-package ws-butler
  :diminish
  :hook prog-mode
  :custom (ws-butler-keep-whitespace-before-point nil))

(use-package format-all
  :commands (format-all-buffer format-all-region)
  :init
  (with-eval-after-load 'meow
    (meow-leader-define-key
     '("= =" . format-all-buffer)
     '("= r" . format-all-region)))
  :config
  (setq-default format-all-formatters
                '(("Bazel" buildifier)
                  ("C" clang-format)
                  ("C++" clang-format)
                  ("CSS" prettier)
                  ("HTML" prettier)
                  ("JSON" prettier)
                  ("JSON5" prettier)
                  ("JavaScript" prettier)
                  ("Lua" stylua)
                  ("Markdown" prettier)
                  ("Python" black)
                  ("Rust" (rustfmt "--edition=2021"))
                  ("Shell" (shfmt "-i" "2"))
                  ("TOML" taplo-fmt)
                  ("YAML" prettier))))

(use-package rg
  :commands rg-menu
  :init
  (with-eval-after-load 'meow
    (meow-leader-define-key '("S" . rg-menu))))

(use-package string-inflection
  :bind
  (("C-c c i" . string-inflection-cycle)
   ("C-c c l" . string-inflection-lower-camelcase)
   ("C-c c c" . string-inflection-camelcase)
   ("C-c c s" . string-inflection-underscore)
   ("C-c c u" . string-inflection-upcase)))

(provide 'init-edit)
;;; init-edit.el ends here
