;;; early-init.el --- -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;; Code:

(let ((orig-value gc-cons-threshold))
  (setq
   gc-cons-threshold most-positive-fixnum
   gc-cons-percentage 0.5)

  (add-hook
   'emacs-startup-hook
   (lambda ()
     ;; 恢复 GC 阈值
     (setq gc-cons-threshold orig-value)
     ;; 闲置时 GC
     (run-with-idle-timer 5 t #'garbage-collect)
     ;; 不聚焦当前窗口时 GC
     (if (boundp 'after-focus-change-function)
         (add-function :after after-focus-change-function
                       (lambda ()
                         (unless (frame-focus-state)
                           (garbage-collect))))
       (add-hook 'after-focus-change-function 'garbage-collect)))))

(setq
 ;; 禁止隐式调整窗口大小
 frame-inhibit-implied-resize t
 ;; 默认使用文本模式
 initial-major-mode 'fundamental-mode
 ;; 不自动启动包
 package-enable-at-startup nil)

;; 加速 GUI 配置
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(setq
 menu-bar-mode nil
 tool-bar-mode nil
 scroll-bar-mode nil)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;;; early-init.el ends here
