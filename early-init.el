;;; early-init.el --- Early Initial File -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;; 包含 GC 和 UI 等配置，主要目的是加速 Emacs 啟動
;;
;;; Code:

(defvar lu--orig-gc-cons-threshold gc-cons-threshold
  "保存原始 GC 閾值.")

(defun lu--gc-when-unfocused ()
  "當窗口不聚焦時執行 GC."
  (unless (frame-focus-state)
    (garbage-collect)))

(defun lu--restore-gc-settings ()
  "恢復 GC 設置並添加空閒時 GC."
  (setq gc-cons-threshold lu--orig-gc-cons-threshold)
  ;; 閒置時 GC
  (run-with-idle-timer 5 t #'garbage-collect)
  ;; 不聚焦當前窗口時 GC
  (add-function :after after-focus-change-function
                #'lu--gc-when-unfocused))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook #'lu--restore-gc-settings)

;; 禁止自動啟動包
(setq package-enable-at-startup nil
      package--init-file-ensured t)

;; 加速 GUI 配置
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(alpha . (95 . 95)) default-frame-alist)

;; 顯示左邊緣，關閉右邊緣
(push '(left-fringe) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)

(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;;; early-init.el ends here
