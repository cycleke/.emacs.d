;;; init-basic.el --- 基础配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  基础配置
;;
;;; Code:

(require 'lu-core)

;; 禁用启动页
(setq inhibit-startup-message t)

;; 以 y/n 代表 yes/no
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add #'yes-or-no-p :override #'y-or-n-p))

;; 不要烦人的错误提示
(setq
 ad-redefinition-action 'accept
 ring-bell-function #'ignore
 visible-bell nil)

;; 不要问 .dir-locals 的问题
(setq enable-local-variables :all)

;; 增加 IO 性能
(setq
 process-adaptive-read-buffering nil
 read-process-output-max (* 1024 1024))

;; 使用字体缓存，避免卡顿
(setq inhibit-compacting-font-caches t)

;; 支持 Emacs 和外部程序的粘贴
(setq
 select-enable-clipboard t
 mouse-yank-at-point t)

;; kill-ring 去重
(setq kill-do-not-save-duplicates t)

;; 滚动设置
(setq
 scroll-step 1
 scroll-conservatively 10000
 fast-but-imprecise-scrolling t)

;; 访问链接文件时访问真实文件
(setq
 find-file-visit-truename t
 vc-follow-symlinks t)

;; 备份文件和锁的设置
(setq
 create-lockfiles t
 make-backup-files nil
 backup-inhibited t)

;; 删除移动到垃圾桶
(setq delete-by-moving-to-trash t)

;; ;; 减少 ping
;; (setq ffap-machine-p-local 'accept
;;       ffap-machine-p-known 'reject
;;       ffap-machine-p-unknown 'reject)

;; 设置光标渲染
(setq x-stretch-cursor t)

;; 关闭自动调节行高
(setq auto-window-vscroll nil)

;; 屏幕外显示对应括号
(setq
 show-paren-context-when-offscreen
 (if (lu-childframe-workable-p)
     'child-frame
   'overlay)
 blink-matching-paren t
 blink-matching-paren-highlight-offscreen t)

;; 输入时不执行跳过字形相关函数
(setq redisplay-skip-fontification-on-input t)

;; 不渲染非聚焦窗口
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; 增强长行处理性能
(setq-default bidi-display-reordering nil)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq
 bidi-inhibit-bpa t
 long-line-threshold 1000
 large-hscroll-threshold 1000
 syntax-wholeline-max 1000)

;; 禁用 ispell 后端
(setq text-mode-ispell-word-completion nil)

;; 自动保存
(setq
 auto-save-default t
 auto-save-include-big-deletions t
 auto-save-list-file-prefix (file-name-concat lu-cache-dir "autosave/")
 tramp-auto-save-directory (file-name-concat lu-cache-dir "tramp-autosave/")
 auto-save-file-name-transforms
 (list
  (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
        (concat auto-save-list-file-prefix "tramp-\\2") t)
  (list ".*" auto-save-list-file-prefix t)))

;; 折行
(setq-default
 word-wrap t
 word-wrap-by-category t
 truncate-lines t
 truncate-partial-width-windows nil
 fill-column 120)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

;; 快捷键
(global-unset-key (kbd "C-h h"))

;; 退出自动杀掉进程
(setq confirm-kill-processes nil)

;; Emacs 退出时确认
(setq confirm-kill-emacs (lambda (_) (y-or-n-p-with-timeout "Quit?" 10 nil)))

;; 优化 pgtk 的 UI 速度
(when (eval (boundp 'pgtk-wait-for-event-timeout))
  (setq pgtk-wait-for-event-timeout 0.001))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 各种自带的 mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(which-key-mode +1)
(blink-cursor-mode -1) ; 关闭指针闪烁
(transient-mark-mode 1) ; 标记高亮
(global-subword-mode 1) ; Word 移动支持 FooBar 的格式
(delete-selection-mode t)
(column-number-mode 1)
(size-indication-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 编码设置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 包管理器
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;; Add the macro generated list of package.el loadpaths to load-path.
(mapc
 #'(lambda (add) (add-to-list 'load-path add))
 (eval-when-compile
   (require 'package)
   (package-initialize)
   ;; Install use-package if not installed yet.
   (unless (package-installed-p 'use-package)
     (package-refresh-contents)
     (package-install 'use-package))
   (let ((package-user-dir-real (file-truename package-user-dir)))
     ;; The reverse is necessary, because outside we mapc
     ;; add-to-list element-by-element, which reverses.
     (nreverse
      (apply #'nconc
             ;; Only keep package.el provided loadpaths.
             (mapcar
              #'(lambda (path)
                  (if (string-prefix-p package-user-dir-real path)
                      (list path)
                    nil))
              load-path))))))

(eval-when-compile
  (require 'use-package))
(use-package bind-key
  :ensure t)
(use-package use-package
  :commands use-package-autoload-keymap)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)

(provide 'init-basic)
;;; init-basic.el ends here
