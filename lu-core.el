;;; lu-core.el --- 核心配置文件 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  核心配置文件
;;
;;; Code:

;; 添加特性
(if (bound-and-true-p module-file-suffix)
    (push 'dynamic-modules features))
(if (fboundp #'json-parse-string)
    (push 'jansson features))
(if (featurep 'native-compile)
    (if (not (native-comp-available-p))
        (delq 'native-compile features)))

(defconst lu-is-mac      (eq system-type 'darwin))
(defconst lu-is-linux    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst lu-is-windows  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst lu-is-bsd      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))

(defvar lu-emacs-dir
  (expand-file-name user-emacs-directory))

(defvar lu-data-dir
  (file-name-concat lu-emacs-dir ".data/"))

(defvar lu-cache-dir
  (file-name-concat lu-emacs-dir ".cache/"))

;; 生成数据和缓存目录
(dolist (dir (list lu-data-dir lu-cache-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; 修改 user-emacs-directory 及相关目录
(setq user-emacs-directory lu-cache-dir)
(setq server-auth-dir (file-name-concat lu-emacs-dir "server/"))
(setq custom-file (file-name-concat lu-data-dir "custom.el"))

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
     (lambda ()
       (setq file-name-handler-alist
             (delete-dups (append file-name-handler-alist orig-value))))))

  (unless noninteractive
    ;; 不显示开始页面以及额外的日志
    (setq inhibit-startup-screen t)
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    (advice-add #'display-startup-screen :override #'ignore)
    ;; 草稿不显示额外信息
    (setq initial-scratch-message nil)

    ;; 加载文件时不打印日志，启动后恢复
    (define-advice load-file (:override (file) silence)
      (load file nil 'nomessage))
    (define-advice startup--load-user-init-file (:before (&rest _) undo-silence)
      (advice-remove #'load-file #'load-file@silence))

    ;; 不渲染 mode line
    (put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
    (setq-default mode-line-format nil)
    (dolist (buf (buffer-list))
      (with-current-buffer buf (setq mode-line-format nil)))
    ;; 关闭启动时闪烁
    (setq-default
     inhibit-redisplay t
     inhibit-message t)
    ;; 恢复默认值
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
  (add-to-list 'native-comp-eln-load-path (file-name-concat lu-cache-dir "eln/"))

  ;; 关闭烦人的告警
  (setq
   native-comp-async-report-warnings-errors nil
   native-comp-warning-on-missing-source nil)

  (unless (boundp 'native-comp-deferred-compilation-deny-list)
    (defvaralias 'native-comp-deferred-compilation-deny-list 'native-comp-jit-compilation-deny-list)))

;; 告警设置
(setq
 ad-redefinition-action 'accept
 warning-suppress-types '((defvaralias)))

;; 关闭大小写不敏感的二次搜索
(setq auto-mode-case-fold nil)

;; 不渲染非聚焦窗口
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; 增强长行处理性能
(setq bidi-inhibit-bpa t)
(setq-default
 bidi-display-reordering 'left-to-right
 bidi-paragraph-direction 'left-to-right)

;; 增加 IO 性能
(setq
 process-adaptive-read-buffering nil
 read-process-output-max (* 1024 1024))

;; 延迟更新 UI
(setq idle-update-delay 1.0)

;; 使用字体缓存，避免卡顿
(setq inhibit-compacting-font-caches t)

;; 禁用起动页
(setq inhibit-startup-message t)

;; 快速滚动
(setq fast-but-imprecise-scrolling t)

;; 减少 ping
(setq ffap-machine-p-known 'reject)

;; 优化 pgtk 的 UI 速度
(when (eval (boundp 'pgtk-wait-for-event-timeout))
  (setq pgtk-wait-for-event-timeout 0.001))

;; 输入时不执行 `fontification_functions`
(setq redisplay-skip-fontification-on-input t)

;; 提升 Windows 下性能
(when (eval (boundp 'w32-get-true-file-attributes))
  (setq
   w32-get-true-file-attributes nil
   w32-pipe-read-delay 0
   w32-pipe-buffer-size (* 64 1024)))

;; 使用 straight.el 管理包
(defvar straight-vc-git-default-clone-depth)
(defvar straight-check-for-modifications)

(setq
 straight-base-dir (file-truename lu-cache-dir)
 straight-build-dir (format "build-%s" emacs-version)
 straight-check-for-modifications nil
 straight-vc-git-default-clone-depth '(1 single-branch))

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously
                          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
                          'silent
                          'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :config
  (setq
   gcmh-idle-delay 'auto
   gcmh-auto-idle-delay-factor 10
   gcmh-high-cons-threshold (* 16 1024 1024)))

(provide 'lu-core)
;;; lu-core.el ends here
