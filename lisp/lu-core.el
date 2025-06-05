;;; lu-core.el --- 核心配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  包含变量申明等代码
;;
;;; Code:

;; 版本判断，最低要求 30.0
(eval-and-compile
  (when (< emacs-major-version 30)
    (user-error (concat "EMACS VERSION IS TOO LOW！！！\n"
                        "Current version " emacs-version ", requires 30.0 or above."))))

;; 添加特性
(if (bound-and-true-p module-file-suffix)
    (push 'dynamic-modules features))
(if (fboundp #'json-parse-string)
    (push 'jansson features))
(if (featurep 'native-compile)
    (if (not (native-comp-available-p))
        (delq 'native-compile features)))

(defconst lu-is-mac (eq system-type 'darwin))
(defconst lu-is-linux (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst lu-is-windows (memq system-type '(cygwin windows-nt ms-dos)))
(defconst lu-is-bsd (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))

(defvar lu-emacs-dir (expand-file-name user-emacs-directory))

(defvar lu-data-dir (file-name-concat lu-emacs-dir ".data/"))

(defvar lu-cache-dir (file-name-concat lu-emacs-dir ".cache/"))

(defvar lu-pre-custom-file (file-name-concat lu-data-dir "pre-custom.el"))

(defvar lu-post-custom-file (file-name-concat lu-data-dir "custom.el"))

(defun lu-temp-buffer ()
  "Switch to the *Lu Temporarily* buffer.
If the buffer doesn't exist, create it first."
  (interactive)
  (pop-to-buffer-same-window (get-buffer-create "*Lu Temporarily*")))

(defun lu-childframe-workable-p ()
  "Whether childframe is workable."
  (and (>= emacs-major-version 26)
       (not noninteractive)
       (not emacs-basic-display)
       (or (display-graphic-p) (featurep 'tty-child-frames))
       (eq (frame-parameter (selected-frame) 'minibuffer) 't)))

(defun lu-indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun lu-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (lu-indent-buffer)
        (message "Indented buffer.")))))

(global-set-key (kbd "C-M-\\") #'lu-indent-region-or-buffer)

(defun lu-byte-compile-site-lisp-if-newer (file)
  "Byte compile FILE in site-lisp directory if it is newer."
  (let ((el-file (concat lu-emacs-dir "/site-lisp/" file ".el"))
        (elc-file (concat lu-emacs-dir "/site-lisp/" file ".elc")))
    (when (file-newer-than-file-p el-file elc-file)
      (byte-compile-file el-file))))

;; 生成数据和缓存目录
(dolist (dir (list lu-data-dir lu-cache-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; 修改 user-emacs-directory 及相关目录
(setq user-emacs-directory lu-cache-dir)
(setq server-auth-dir (file-name-concat lu-emacs-dir "server/"))
(setq custom-file lu-post-custom-file)

;; 启动加速
(unless (daemonp)
  ;; 优化 file-name-handler-alist
  (let ((orig-value (default-toplevel-value 'file-name-handler-alist)))
    (setq file-name-handler-alist
          (if (eval-when-compile
                (locate-file-internal "calc-loaddefs.el" load-path))
              nil
            (list (rassq 'jka-compr-handler orig-value))))
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    ;; 启动后恢复
    (add-hook
     'emacs-startup-hook
     (lambda () (setq file-name-handler-alist (delete-dups (append file-name-handler-alist orig-value))))))

  (unless noninteractive
    ;; 不显示开始页面以及额外的日志
    (setq inhibit-startup-screen t)
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    (advice-add #'display-startup-screen :override #'ignore)

    ;; 草稿页不显示额外信息
    (setq initial-scratch-message nil)

    ;; 加载文件时不打印日志，启动后恢复
    (define-advice load-file (:override (file) silence)
      (load file nil 'nomessage))
    (define-advice startup--load-user-init-file (:before (&rest _) undo-silence)
      (advice-remove #'load-file #'load-file@silence))

    ;; 不渲染 mode line，启动后恢复
    (put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
    (setq-default mode-line-format nil)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (setq mode-line-format nil)))

    ;; 让窗口启动更平滑，关闭启动时闪烁
    (setq frame-inhibit-implied-resize t)
    (setq-default
     inhibit-redisplay t
     inhibit-message t)

    ;; 恢复
    (add-hook
     'window-setup-hook
     (lambda ()
       (setq-default
        inhibit-redisplay nil
        inhibit-message nil)
       (redraw-frame)
       (unless (default-toplevel-value 'mode-line-format)
         (setq-default mode-line-format (get 'mode-line-format 'initial-value)))))

    (unless lu-is-mac
      (setq command-line-ns-option-alist nil))
    (unless (memq initial-window-system '(x pgtk))
      (setq command-line-x-option-alist nil))))

;; Native compilation 支持
(when (boundp 'native-comp-eln-load-path)
  (add-to-list 'native-comp-eln-load-path (file-name-concat lu-cache-dir "eln-cache/"))

  ;; 关闭烦人的告警
  (setq
   native-comp-async-report-warnings-errors nil
   native-comp-warning-on-missing-source nil)

  (unless (boundp 'native-comp-deferred-compilation-deny-list)
    (defvaralias 'native-comp-deferred-compilation-deny-list 'native-comp-jit-compilation-deny-list)))

(provide 'lu-core)
;;; lu-core.el ends here
