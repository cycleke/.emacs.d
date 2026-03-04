;;; init-edit.el --- 編輯相關配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;; 編輯相關配置
;;
;;; Code:

;; 文件末尾添加空白行
(setq require-final-newline t)

;; 設置 sentence-end 可以識別中文標點
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

;; 縮進和 Tab 設置
(setq-default indent-tabs-mode nil
              tab-width 4
              tab-always-indent nil)
(setq tabify-regexp "^\\t* [ \\t]+")

;; 保持補全的原始大小寫
(setq dabbrev-case-replace nil)

;; Flymake 診斷
(use-package flymake
  :ensure nil
  :hook (emacs-lisp-mode eglot-managed-mode)
  :preface
  (defun lu-toggle-flymake-diagnostics-buffer ()
    "切換 Flymake 診斷 Buffer."
    (interactive)
    (let ((buffer-name (flymake--diagnostics-buffer-name)))
      (if-let ((window (get-buffer-window buffer-name)))
          (delete-window window)
        (flymake-show-buffer-diagnostics))))
  :bind ("C-c f" . lu-toggle-flymake-diagnostics-buffer))

;; 自動加載文件
(use-package auto-revert
  :ensure nil
  :hook ((after-init . global-auto-revert-mode)
         (dired-mode . auto-revert-mode))
  :bind ("C-M-g" . revert-buffer)
  :custom
  (revert-without-query
   '("\\.pdf\\'" "\\.png\\'" "\\.jpg\\'" "\\.gif\\'"
     "\\.svg\\'" "\\.out\\'" "\\.log\\'")))

;; 註釋
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; 刪除空格
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode))

;; 多光標編輯
(use-package iedit
  :bind ("C-;" . iedit-mode))

(use-package smartparens
  :diminish
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config)
  ;; 添加中文括號支持
  (sp-pair "【" "】")
  (sp-pair "「" "」")
  (sp-pair "『" "』")
  (sp-pair "（" "）")
  (sp-pair "《" "》")
  (sp-pair "〈" "〉"))

;; 撤銷
(use-package vundo
  :bind ("C-x u" . vundo))

;; 快速跳轉
(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom (aw-scope 'frame))

;; 保存最近打開文件
(use-package recentf
  :ensure nil
  :hook ((after-init . recentf-mode)
         (kill-emacs . recentf-cleanup))
  :custom
  (recentf-auto-cleanup 600)
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 20)
  (recentf-save-file (file-name-concat lu-cache-dir "recentf"))
  (recentf-exclude
   '(,recentf-save-file
     ,package-user-dir
     ".*?autoloads\\.el\\'"
     "/loaddefs\\.el\\'"
     "\\.elc\\'"

     ;; 臨時文件
     "/tmp/"
     "/var/tmp/"
     "/AppData/Local/Temp/"
     "/var/folders/.+"

     ;; 版本控制
     "/\\.git/"
     "COMMIT_EDITMSG\\'"
     "MERGE_MSG\\'"
     "git-rebase-todo\\'"

     ;; TAGS
     "/\\.?[tT][aA][gG][sG]\\'"

     ;; 二進制媒體文件
     "\\.\\(?:gz\\|tar\\|zip\\|rar\\|7z\\)\\'"
     "\\.\\(?:gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\|ico\\|webp\\)\\'"
     "\\.\\(?:mp[34]\\|avi\\|mkv\\|mov\\|webm\\|wav\\|flac\\|ogg\\)\\'"
     "\\.\\(?:pdf\\|docx?\\|xlsx?\\|pptx?\\|od[tfsp]\\)\\'"))
  (recentf-keep '(file-remote-p file-readable-p)))


;; 保存歷史記錄
(use-package savehist
  :ensure nil
  :hook ((after-init . savehist-mode)
         (kill-emacs . savehist-save))
  :custom
  (savehist-autosave-interval 120)
  (savehist-file (file-name-concat lu-cache-dir "savehist"))
  (savehist-additional-variables
   '(kill-ring
     mark-ring
     global-mark-ring
     search-ring
     regexp-search-ring
     extended-command-history
     shell-command-history))
  (savehist-ignored-variables '(kill-ring))

  (history-length 1000)
  (enable-recursive-minibuffers t))

;; 保存光標位置
(use-package save-place
  :ensure nil
  :hook (after-init . save-place-mode)
  :custom
  (save-place-file (file-name-concat lu-cache-dir "saveplace")))

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
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol))

(use-package so-long
  :hook (after-init . global-so-long-mode)
  :config
  (setq so-long-threshold
        (if (fboundp 'buffer-line-statistics)
            (if (featurep 'native-compile) 1000 5000)
          400))

  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))

  (setq so-long-minor-modes
        (delq 'font-lock-mode so-long-minor-modes))
  (setq so-long-minor-modes
        (delq 'display-line-numbers-mode so-long-minor-modes))
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
  (defun lu-update-formatters (&rest formatters)
    "批量更新 `format-all-formatters' 的全局默認值。
FORMATTERS 格式應為 (language-id formatter-or-list) 的序列。"
    (let ((current-settings (default-value 'format-all-formatters)))
      (dolist (item formatters)
        (let ((lang (car item))
              (fmt (cadr item)))
          (setq current-settings (cons (list lang fmt)
                                       (assoc-delete-all lang current-settings)))))
      (setq-default format-all-formatters current-settings)))

  (setq-default format-all-formatters
                '(("Bazel" buildifier)
                  ("C" (clang-format "--fallback-style=Google"))
                  ("C++" (clang-format "--fallback-style=Google"))
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
                  ("TOML" taplo-fmt))))

(use-package rg
  :commands rg-menu
  :init
  (with-eval-after-load 'meow
    (meow-leader-define-key '("S" . rg-menu))))

(provide 'init-edit)
;;; init-edit.el ends here
