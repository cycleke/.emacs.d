;;; init-os.el --- 操作系统相关配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  操作系统相关配置
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows 相关配置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when lu-is-windows
  ;; cmdproxy.exe 使用 GBK 编码
  (set-default
   'process-coding-system-alist
   '(("[pP][lL][iI][nN][kK]" gbk-dos . gbk-dos) ("[cC][mM][dD][pP][rR][oO][xX][yY]" gbk-dos . gbk-dos)))
  ;; Windows 剪贴板需要用 utf-16-le
  (set-next-selection-coding-system 'utf-16-le)
  (set-selection-coding-system 'utf-16-le)
  (set-clipboard-coding-system 'utf-16-le)
  ;; 提升 Windows 下性能
  (when (eval (boundp 'w32-get-true-file-attributes))
    (setq
     w32-get-true-file-attributes nil
     w32-pipe-read-delay 0
     w32-pipe-buffer-size (* 64 1024))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macOS 相关配置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when lu-is-mac
  ;; macOS 下恢复 menu-bar
  (dolist (hook '(window-setup-hook after-make-frame-functions))
    (add-hook
     hook
     (lambda (&optional frame)
       (when-let* (frame
                   ,(or frame (selected-frame)))
         (when (display-graphic-p frame)
           (set-frame-parameter frame 'menu-bar-lines 1))))))

  (setq
   locate-command "mdfind" ; 使用 spotlight 作为 locate 后端
   ns-use-native-fullscreen t ; 使用原生全屏
   mac-option-modifier 'meta ; Option 键视为 Meta 键
   ns-pop-up-frames nil) ; 默认不创建新窗口

  ;; 触控板/鼠标设置
  (setq
   mac-redisplay-dont-reset-vscroll t
   mac-mouse-wheel-smooth-scroll nil))

(provide 'init-os)
;;; init-os.el ends here
