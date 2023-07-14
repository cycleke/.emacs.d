;;; os/macos/config.el --- macOS 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  macOS 配置
;;
;;; Code:

;; macOS 下恢复 menu-bar
(when (eval 'lu-is-mac)
  (dolist (hook '(window-setup-hook after-make-frame-functions))
    (add-hook
     hook
     (lambda (&optional frame)
       (when-let
           (frame (or frame
                      (selected-frame)))
         (when (display-graphic-p frame)
           (set-frame-parameter frame 'menu-bar-lines 1)))))))

;; 使用 spotlight 作为 locate 后端
(setq locate-command "mdfind")

;; 使用原生全屏
(setq ns-use-native-fullscreen nil)

;; 默认不创建新窗口
(setq ns-pop-up-frames nil)

;; 触控板/鼠标设置
(setq
 mac-redisplay-dont-reset-vscroll t
 mac-mouse-wheel-smooth-scroll nil)

(and (or (daemonp)
         (display-graphic-p))
     (require 'ns-auto-titlebar nil t)
     (ns-auto-titlebar-mode +1))

;;; os/macos/config.el ends here
