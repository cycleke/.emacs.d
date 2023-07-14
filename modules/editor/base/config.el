;;; editor/base/config.el --- 基础配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  基础配置
;;
;;; Code:

;; 访问链接文件时访问真实文件
(setq
 find-file-visit-truename t
 vc-follow-symlinks t)

;; 备份文件和锁的设置
(setq
 create-lockfiles nil
 make-backup-files nil
 version-control t
 backup-by-copying t
 kept-old-versions 5
 kept-new-versions 5
 backup-directory-alist (list (cons "." (file-name-concat lu-cache-dir "backup/")))
 tramp-backup-directory-alist backup-directory-alist)

;; 自动保存
(setq
 auto-save-default t
 auto-save-include-big-deletions t
 auto-save-list-file-prefix (file-name-concat lu-cache-dir "autosave/")
 tramp-auto-save-directory  (file-name-concat lu-cache-dir "tramp-autosave/")
 auto-save-file-name-transforms
 (list
  (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
        (concat auto-save-list-file-prefix "tramp-\\2") t)
  (list ".*" auto-save-list-file-prefix t)))

;; 缩进和 Tab 设置
(setq-default
 indent-tabs-mode nil
 tab-width 4
 tab-always-indent nil)
(setq tabify-regexp "^\t* [ \t]+")

;; 行长度
(setq-default fill-column 120)

;; 折行
(setq-default
 word-wrap t
 word-wrap-by-category t
 truncate-lines t
 truncate-partial-width-windows nil)
(add-hook 'text-mode-hook #'visual-line-mode)

;; 句末空格设置
(setq-default sentence-end-double-space nil)

;; 文件末尾添加空白行
(setq require-final-newline t)

;; 删除移动到垃圾桶
(setq delete-by-moving-to-trash t)

;; kill-ring 去重
(setq kill-do-not-save-duplicates t)

;; 关闭自动调节行高
(setq auto-window-vscroll nil)

;; 不要烦人的错误提示
(setq
 ad-redefinition-action 'accept
 ring-bell-function #'ignore
 visible-bell nil)

;; 支持 Emacs 和外部程序的粘贴
(setq select-enable-clipboard t)

;; 退出自动杀掉进程
(setq confirm-kill-processes nil)

;; 抑制编译警告信息
(setq byte-compile-warnings '(not free-vars obsolete cl-functions))

;; 以 y/n 代表 yes/no
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add #'yes-or-no-p :override #'y-or-n-p))

;; Emacs 退出时确认
(setq confirm-kill-emacs
      (lambda (_) (y-or-n-p-with-timeout "是否退出 Emacs ?" 10 "n")))

;; 编码设置
(set-language-environment "UTF-8")
(setq default-input-method nil)
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(set-coding-system-priority 'utf-8 'chinese-gb18030 'gb2312)

(when (eval 'lu-is-windows)
  ;; Windows 剪贴板需要用 utf-16-le
  (set-next-selection-coding-system 'utf-16-le)
  (set-selection-coding-system 'utf-16-le)
  (set-clipboard-coding-system 'utf-16-le))

;;; editor/base/config.el ends here
