;;; init-os.el --- 操作系統相關配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;; 操作系統相關配置
;;
;;; Code:

;; Windows 相關配置
(when lu-is-windows
  ;; 文件名編碼使用 GBK，確保中文文件名正確顯示
  (setq file-name-coding-system 'gbk)

  ;; 進程編碼設置
  (set-default 'process-coding-system-alist
               '(("rg\\.exe" utf-8 . gbk)
                 ("es\\.exe" gbk . gbk)
                 ("explorer\\.exe" gbk . gbk)
                 ("[pP][lL][iI][nN][kK]" gbk-dos . gbk-dos)
                 ("[cC][mM][dD][pP][rR][oO][xX][yY]" gbk-dos . gbk-dos)))

  ;; Windows 剪貼板需要用 utf-16-le
  (set-next-selection-coding-system 'utf-16-le)
  (set-selection-coding-system 'utf-16-le)
  (set-clipboard-coding-system 'utf-16-le)

  ;; 提升 Windows 下性能
  (when (boundp 'w32-get-true-file-attributes)
    (setq w32-get-true-file-attributes nil
          w32-pipe-read-delay 0
          w32-pipe-buffer-size (* 64 1024))))

;; macOS 相關配置
(when lu-is-mac
  ;; macOS 下恢復 menu-bar
  (defun lu--restore-menu-bar (&optional frame)
    "在圖形界面下恢復 FRAME 的 menu-bar."
    (when-let* ((frm (or frame (selected-frame))))
      (when (display-graphic-p frm)
        (set-frame-parameter frm 'menu-bar-lines 1))))

  (dolist (hook '(window-setup-hook after-make-frame-functions))
    (add-hook hook #'lu--restore-menu-bar))

  (setq locate-command "mdfind"
        ns-use-native-fullscreen t
        mac-option-modifier 'meta
        ns-pop-up-frames nil)

  ;; 觸控板/鼠標設置
  (setq mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smooth-scroll nil))

(provide 'init-os)
;;; init-os.el ends here
