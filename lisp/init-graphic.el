;; init-graphic.el ---	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; 图形化插件特殊设置
(if (not (display-graphic-p))
	(add-hook 'after-make-frame-functions
			  (lambda (new-frame)
				(select-frame new-frame)
				(dolist (elisp-code graphic-only-plugins-setting)
				  (eval elisp-code))))
	(dolist (elisp-code graphic-only-plugins-setting)
				  (eval elisp-code)))

(if (display-graphic-p)
    (message "检测到当前环境为图形环境，可以正常使用。")
  (message "检测到当前环境为字符环境，部分插件未启用。"))

(provide 'init-graphic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-graphic.el ends here
