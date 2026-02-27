;;; init-basic.el --- 基礎配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;; 基礎配置
;;
;;; Code:

(require 'lu-core)

;; 禁用啟動頁
(setq inhibit-startup-message t)

;; 以 y/n 代表 yes/no
(setq use-short-answers t)

;; 減少告警
(setq ad-redefinition-action 'accept
      ring-bell-function #'ignore
      visible-bell nil)

;; 不要問 .dir-locals 的問題
(setq enable-local-variables :all)

;; 增加 IO 性能
(setq process-adaptive-read-buffering nil
      read-process-output-max (* 1024 1024))

;; 使用字體緩存，避免卡頓
(setq inhibit-compacting-font-caches t)

;; 支持 Emacs 和外部程序的粘貼
(setq select-enable-clipboard t
      mouse-yank-at-point t)

;; kill-ring 去重
(setq kill-do-not-save-duplicates t)

;; 滾動設置
(setq scroll-step 1
      scroll-conservatively 10000
      fast-but-imprecise-scrolling t)

;; 訪問鏈接文件
(setq vc-follow-symlinks t
      find-file-visit-truename nil)

;; 備份文件和鎖的設置
(setq create-lockfiles t
      make-backup-files t)

(setq backup-directory-alist
      `((".*" . ,(file-name-concat lu-cache-dir "backup")))
      backup-by-copying t
      version-control t
      delete-old-versions t)

(setq lock-file-name-transforms
      `((".*" ,(file-name-concat lu-cache-dir "lock-file") t)))

;; 刪除移動到垃圾桶
(setq delete-by-moving-to-trash t)

;; 時間格式
(setq system-time-locale "C"
      calendar-date-style 'iso
      calendar-day-abbrev-array
      ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]
      calendar-day-name-array
      ["日曜日" "月曜日" "火曜日" "水曜日" "木曜日" "金曜日" "土曜日"]
      calendar-month-name-array
      ["一月" "二月" "三月" "四月" "五月" "六月"
       "七月" "八月" "九月" "十月" "冬月" "臘月"])

;; 減少 ffap 的 ping 操作
;; (setq ffap-machine-p-local 'accept
;;       ffap-machine-p-known 'reject
;;       ffap-machine-p-unknown 'reject)

;; 設置光標渲染
(setq x-stretch-cursor t)

;; 關閉自動調節行高
(setq auto-window-vscroll nil)

;; 屏幕外顯示對應括號
(setq show-paren-context-when-offscreen
      (if (lu-childframe-workable-p)
          'child-frame
        'overlay)
      blink-matching-paren t
      blink-matching-paren-highlight-offscreen t)

;; 設置 Elisp 高亮
(setq elisp-fontify-semantically t)

;; 輸入時不執行跳過字形相關函數
(setq redisplay-skip-fontification-on-input t)

;; 不渲染非聚焦窗口
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; 增強長行處理性能
(setq-default bidi-display-reordering nil)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;; 禁用 ispell 後端
(setq text-mode-ispell-word-completion nil)

;; 自動保存
(setq auto-save-default t
      auto-save-include-big-deletions t
      auto-save-list-file-prefix (file-name-concat lu-cache-dir "autosave/")
      tramp-auto-save-directory (file-name-concat lu-cache-dir "tramp-autosave/")
      auto-save-file-name-transforms
      (list
       (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
             (file-name-concat auto-save-list-file-prefix "tramp-\\2") t)
       (list ".*" auto-save-list-file-prefix t)))

;; 折行
(setq-default word-wrap t
              word-wrap-by-category t
              truncate-lines t
              truncate-partial-width-windows nil
              fill-column 120)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

;; 快捷鍵
(global-unset-key (kbd "C-h h"))

;; 退出自動殺掉進程
(setq confirm-kill-processes nil)

;; Emacs 退出時確認
(defun lu--confirm-quit (_)
  "確認是否退出 Emacs."
  (y-or-n-p-with-timeout "Quit?" 10 nil))

(setq confirm-kill-emacs #'lu--confirm-quit)

;; 優化 pgtk 的 UI 速度
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; 各種自帶的 mode
(which-key-mode +1)
(blink-cursor-mode -1)
(transient-mark-mode +1)
(global-subword-mode +1)
(delete-selection-mode +1)
(column-number-mode +1)
(size-indication-mode +1)

;; 編碼設置
(setq default-input-method nil)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-next-selection-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(set-coding-system-priority 'utf-8 'chinese-gb18030 'gb2312)

;; 包管理器
(eval-and-compile
  (require 'package)
  (package-initialize)
  (require 'use-package))

(setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))
      use-package-always-defer t
      use-package-always-ensure t)

(use-package bind-key)

(provide 'init-basic)
;;; init-basic.el ends here
