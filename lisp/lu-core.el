;;; lu-core.el --- 核心配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;; 包含變量聲明等代碼
;;
;;; Code:

;; 版本判斷，最低要求 30.0
(eval-and-compile
  (when (< emacs-major-version 30)
    (user-error "EMACS 版本過低！！！\n當前版本 %s，需要 30.0 或更高版本" emacs-version)))

;; 添加特性
(when (bound-and-true-p module-file-suffix)
  (push 'dynamic-modules features))
(when (fboundp #'json-parse-string)
  (push 'jansson features))
(when (featurep 'native-compile)
  (unless (native-comp-available-p)
    (setq features (delq 'native-compile features))))

(defconst lu-is-mac (eq system-type 'darwin))
(defconst lu-is-linux (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst lu-is-windows (memq system-type '(cygwin windows-nt ms-dos)))
(defconst lu-is-bsd (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))

(defvar lu-emacs-dir (expand-file-name user-emacs-directory))

(defvar lu-data-dir (file-name-concat lu-emacs-dir ".data/"))
(defvar lu-cache-dir (file-name-concat lu-emacs-dir ".cache/"))

(defvar lu-pre-custom-file (file-name-concat lu-data-dir "pre-custom.el"))
(defvar lu-custom-file (file-name-concat lu-data-dir "custom.el"))

(defun lu-temp-buffer ()
  "切換至 *Lu Temporarily* 緩衝區.
若該緩衝區不存在，則先創建之."
  (interactive)
  (pop-to-buffer-same-window (get-buffer-create "*Lu Temporarily*")))

(defun lu-open-custom-file ()
  "打開自定義文件（custom.el）."
  (interactive)
  (find-file lu-custom-file))

(defun lu-childframe-workable-p ()
  "判斷子視圖（childframe）是否可用."
  (and (>= emacs-major-version 26)
       (not noninteractive)
       (not emacs-basic-display)
       (or (display-graphic-p) (featurep 'tty-child-frames))
       (eq (frame-parameter (selected-frame) 'minibuffer) 't)))

(defun lu-indent-buffer ()
  "縮進當前緩衝區."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun lu-indent-region-or-buffer ()
  "縮進選區或整個緩衝區."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "已縮進選區"))
      (lu-indent-buffer)
      (message "已縮進緩衝區"))))

(global-set-key (kbd "C-M-\\") #'lu-indent-region-or-buffer)

(defun lu-insert-zero-width-space ()
  "插入零寬空格字符."
  (interactive)
  (insert "\u200B"))

(defun lu-update-bytecode (file-path)
  "若 FILE-PATH 對應的 .el 文件比 .elc 文件新，或者 .elc 不存在，則進行編譯."
  (when-let* ((base-name (file-name-sans-extension file-path))
              (el-file (concat base-name ".el"))
              (elc-file (concat base-name ".elc")))
    (when (and (file-exists-p el-file)
               (file-newer-than-file-p el-file elc-file))
      (condition-case err
          (byte-compile-file el-file)
        (error
         (message "字節碼編譯失敗 %s: %s" el-file (error-message-string err)))))))

(defun lu-update-site-lisp-bytecode (file-name)
  "若 site-lisp/ 目錄中 FILE-NAME 對應的 .el 文件需要更新（或未編譯），則編譯.
此函數會自動拼接 `lu-emacs-dir'/site-lisp/ 路徑."
  (when-let* ((full-path
               (file-name-concat lu-emacs-dir "site-lisp" file-name)))
    (lu-update-bytecode full-path)))

;; 生成數據和緩存目錄
(dolist (dir (list lu-data-dir lu-cache-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; 修改 user-emacs-directory 及相關目錄
(setq user-emacs-directory lu-cache-dir)
(setq server-auth-dir (file-name-concat lu-emacs-dir "server/"))
(setq custom-file lu-custom-file)

;; 啟動加速
(unless (daemonp)
  ;; 優化 file-name-handler-alist
  (defvar lu--orig-file-name-handler-alist
    (default-toplevel-value 'file-name-handler-alist)
    "保存原始 file-name-handler-alist 用於啟動後恢復.")

  (defun lu--restore-file-name-handler-alist ()
    "恢復 file-name-handler-alist."
    (setq file-name-handler-alist
          (delete-dups (append file-name-handler-alist
                               lu--orig-file-name-handler-alist))))

  (setq file-name-handler-alist
        (if (eval-when-compile
              (locate-file-internal "calc-loaddefs.el" load-path))
            nil
          (list (rassq 'jka-compr-handler lu--orig-file-name-handler-alist))))
  (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
  ;; 啟動後恢復
  (add-hook 'emacs-startup-hook #'lu--restore-file-name-handler-alist)

  (unless noninteractive
    ;; 不顯示開始頁面以及額外的日誌
    (setq inhibit-startup-screen t)
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    (advice-add #'display-startup-screen :override #'ignore)

    ;; 草稿頁不顯示額外信息
    (setq initial-scratch-message nil)

    ;; 加載文件時不打印日誌，啟動後恢復
    (define-advice load-file (:override (file) silence)
      (load file nil 'nomessage))
    (define-advice startup--load-user-init-file (:before (&rest _) undo-silence)
      (advice-remove #'load-file #'load-file@silence))

    ;; 不渲染 mode line，啟動後恢復
    (put 'mode-line-format
         'initial-value (default-toplevel-value 'mode-line-format))
    (setq-default mode-line-format nil)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (setq mode-line-format nil)))

    ;; 讓窗口啟動更平滑，關閉啟動時閃爍
    (setq frame-inhibit-implied-resize t)
    (setq-default inhibit-redisplay t
                  inhibit-message t)

    ;; 恢復
    (defun lu--restore-display-settings ()
      "恢復顯示設置."
      (setq-default inhibit-redisplay nil
                    inhibit-message nil)
      (redraw-frame)
      (unless (default-toplevel-value 'mode-line-format)
        (setq-default mode-line-format (get 'mode-line-format 'initial-value))))

    (add-hook 'window-setup-hook #'lu--restore-display-settings)

    (unless lu-is-mac
      (setq command-line-ns-option-alist nil))
    (unless (memq initial-window-system '(x pgtk))
      (setq command-line-x-option-alist nil))))

;; Native compilation 支持
(when (boundp 'native-comp-eln-load-path)
  (add-to-list 'native-comp-eln-load-path
               (file-name-concat lu-cache-dir "eln-cache/"))

  ;; 關閉煩人的告警
  (setq native-comp-async-report-warnings-errors nil
        native-comp-warning-on-missing-source nil)

  (unless (boundp 'native-comp-deferred-compilation-deny-list)
    (defvaralias 'native-comp-deferred-compilation-deny-list
      'native-comp-jit-compilation-deny-list)))

(provide 'lu-core)
;;; lu-core.el ends here
