;;; early-init.el --- Early Initial File -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;; 包含 GC 和 UI 等配置，主要目的是加速 Emacs 启动
;;
;;; Code:

(let ((orig-value gc-cons-threshold))
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6)

  (add-hook
   'emacs-startup-hook
   (lambda ()
     ;; 恢复 GC 阈值
     (setq gc-cons-threshold orig-value)
     ;; 闲置时 GC
     (run-with-idle-timer 5 t #'garbage-collect)
     ;; 不聚焦当前窗口时 GC
     (add-function :after after-focus-change-function
                   (lambda ()
                     (unless (frame-focus-state)
                       (garbage-collect)))))))

;; 禁止自动启动包
(setq package-enable-at-startup nil
      package--init-file-ensured t)

;; 加速 GUI 配置
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(alpha . (95 . 95)) default-frame-alist)

;; 显示左边缘，关闭右边缘
(push '(left-fringe) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)

(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;;; early-init.el ends here
