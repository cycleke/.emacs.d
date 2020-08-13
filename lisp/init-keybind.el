;;; init-keybind.el --- Initialize the key bind.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(which-key-add-key-based-replacements
  "M-SPC o" " 开启 "
  "M-SPC t" " 切换 "
  "M-SPC w" " 窗口 "
  "M-SPC c" " 代码 "
  "M-SPC p" " 项目 "
  "M-SPC w P" " 交换窗口 - 上 "
  "M-SPC w N" " 交换窗口 - 下 "
  "M-SPC w F" " 交换窗口 - 右 "
  "M-SPC w B" " 交换窗口 - 左 ")
(general-define-key "M-SPC" 'leader-key)
(bind-key "t p" 'toggle-proxy leader-key)

(use-package hydra
  :ensure t
  :commands
  (hydra-default-pre
	 hydra-keyboard-quit
	 hydra--call-interactively-remap-maybe
	 hydra-show-hint
	 hydra-set-transient-map))

(use-package major-mode-hydra :ensure t:after hydra)

(use-package pretty-hydra
  :defines (display-line-numbers-mode linum-mode)
  :functions set-package-archives
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
				                              &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
	        (height (or height 1.0))
	        (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (display-graphic-p) icon-type icon-name)
	       (let ((f (intern (format "all-the-icons-%s" icon-type))))
	         (when (fboundp f)
	           (concat
	            (apply f (list icon-name :face face :height height :v-adjust v-adjust))
	            " "))))
       (propertize title 'face face)))))

;; Global toggles
(pretty-hydra-define toggles-hydra
  (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on")
					:color amaranth :quit-key "q")
  ("Basic"
   (("n" (if (fboundp 'display-line-numbers-mode)
	           (display-line-numbers-mode (if display-line-numbers-mode -1 1))
	         (linum-mode (if linum-mode -1 1)))
     "line number" :toggle (if (fboundp 'display-line-numbers-mode)
				                       display-line-numbers-mode
			                       linum-mode))
    ("a" aggressive-indent-mode "aggressive indent" :toggle t)
    ("h" hungry-delete-mode "hungry delete" :toggle t)
    ("e" electric-pair-mode "electric pair" :toggle t)
    ("c" flyspell-mode "spell check" :toggle t)
    ("S" prettify-symbols-mode "pretty symbol" :toggle t)
    ("L" page-break-lines-mode "page break lines" :toggle t)
    ("M" doom-modeline-mode "modern mode-line" :toggle t))
   "Highlight"
   (("l" global-hl-line-mode "line" :toggle t)
    ("P" show-paren-mode "paren" :toggle t)
    ("s" symbol-overlay-mode "symbol" :toggle t)
    ("r" rainbow-mode "rainbow" :toggle t)
    ("w" (setq show-trailing-whitespace (not show-trailing-whitespace))
     "whitespace" :toggle show-trailing-whitespace)
    ("d" rainbow-delimiters-mode "delimiter" :toggle t)
    ("i" highlight-indent-guides-mode "indent" :toggle t)
    ("T" hl-todo-mode "todo" :toggle t))
   "Coding"
   (("f" flycheck-mode "flycheck" :toggle t)
    ("F" flymake-mode "flymake" :toggle t)
    ("o" origami-mode "folding" :toggle t)
    ("O" hs-minor-mode "hideshow" :toggle t)
    ("u" subword-mode "subword" :toggle t)
    ("W" which-function-mode "which function" :toggle t)
    ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))
   "Version Control"
   (("v" diff-hl-mode "gutter" :toggle t)
    ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
    ("m" diff-hl-margin-mode "margin gutter" :toggle t)
    ("D" diff-hl-dired-mode "dired gutter" :toggle t))
   "Package Archive"
   (("p m" (progn (setq my-package-archives 'melpa)
		              (set-package-archives my-package-archives))
     "melpa" :toggle (eq my-package-archives 'melpa))
    ("p i" (progn (setq my-package-archives 'melpa-mirror)
		              (set-package-archives my-package-archives))
     "melpa mirror" :toggle (eq my-package-archives 'melpa-mirror))
    ("p c" (progn (setq my-package-archives 'emacs-china)
		              (set-package-archives my-package-archives))
     "emacs china" :toggle (eq my-package-archives 'emacs-china))
    ("p n" (progn (setq my-package-archives 'netease)
		              (set-package-archives my-package-archives))
     "netease" :toggle (eq my-package-archives 'netease))
    ("p t" (progn (setq my-package-archives 'tencent)
		              (set-package-archives my-package-archives))
     "tencent" :toggle (eq my-package-archives 'tencent))
    ("p u" (progn (setq my-package-archives 'tuna)
		              (set-package-archives my-package-archives))
     "tuna" :toggle (eq my-package-archives 'tuna)))))

(defhydra awesome-fast-switch (:hint nil)
  "
 ^^^^Fast Move             ^^^^Tab                    ^^Search            ^^Misc
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
   ^_k_^   prev group    | _C-a_^^     select first | _b_ search buffer | _C-k_   kill buffer
 _h_   _l_  switch tab   | _C-e_^^     select last  | _g_ search group  | _C-S-k_ kill others in group
   ^_j_^   next group    | _C-j_^^     ace jump     | ^^                | ^^
 ^^0 ~ 9^^ select window | _C-h_/_C-l_ move current | ^^                | ^^
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
"
  ("h" awesome-tab-backward-tab)
  ("j" awesome-tab-forward-group)
  ("k" awesome-tab-backward-group)
  ("l" awesome-tab-forward-tab)
  ("0" my-select-window)
  ("1" my-select-window)
  ("2" my-select-window)
  ("3" my-select-window)
  ("4" my-select-window)
  ("5" my-select-window)
  ("6" my-select-window)
  ("7" my-select-window)
  ("8" my-select-window)
  ("9" my-select-window)
  ("C-a" awesome-tab-select-beg-tab)
  ("C-e" awesome-tab-select-end-tab)
  ("C-j" awesome-tab-ace-jump)
  ("C-h" awesome-tab-move-current-tab-to-left)
  ("C-l" awesome-tab-move-current-tab-to-right)
  ("b" ivy-switch-buffer)
  ("g" awesome-tab-counsel-switch-group)
  ("C-k" kill-current-buffer)
  ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
  ("q" nil "quit"))

 (defhydra hydra-window-menu ()
  "
							^ 窗口管理器 ^
-----------------------------------------------------------------
[_0_] ^ 关闭窗格 ^				[_F_] ^ 全屏模式 ^		[_K_] ^↑+^		[_k_] ^go ↑^
[_1_] ^ 关闭其他窗格 ^			[_r_] ^ 旋转交换 ^		[_J_] ^↓+^		[_j_] ^go ↓^
[_2_] ^ 新建窗格 (垂直)^		[_s_] ^ 选择交换 ^		[_H_] ^←+^		[_h_] ^go ←^
[_3_] ^ 新建窗格 (水平)^		[_b_] ^ 平均铺开 ^		[_L_] ^→+^		[_l_] ^go →^
"
  ("0" delete-window nil)
  ("1" delete-other-window nil :color blue)
  ("2" split-window-vertically nil)
  ("3" split-window-horizontally nil)
  ("F" toggle-frame-fullscreen nil :color blue)
  ("r" rotate-window nil)
  ("s" ace-swap-window nil :color blue)
  ("b" balance-windows nil :color blue)
  ("H" shrink-window-horizontally nil)
  ("J" enlarge-window nil)
  ("K" shrink-window nil)
  ("L" enlarge-window-horizontally nil)
  ("h" windmove-left nil)
  ("j" windmove-down nil)
  ("k" windmove-up nil)
  ("l" windmove-right nil)
  ("q" nil "QUIT" :color blue))

(pretty-hydra-define eaf-hydra (:color blue)
  ("Emacs"
   (("s" eaf-search-it " 立即搜索 ")
	  ("b" eaf-open-browser " 打开网页 ")
	  ("h" eaf-open-browser-with-history " 历史记录 ")
	  ("e" eaf-proxy-open_download_manage_page " 下载管理 ")
	  ("m" eaf-open-bookmark " 打开书签 "))
   "Application"
   (("o" eaf-open " 智能 Open")
    ("c" eaf-open-camera " 打开摄像 ")
    ("p" eaf-open-mindmap " 思维导图 (O)")
    ("l" eaf-create-mindmap " 思维导图 (N)")
    ("r" eaf-restart-process " 刷新 EAF"))
   "Framwork"
   (("t" eaf-open-terminal " 打开终端 ")
    ("f" eaf-file-send-qrcode " 隔空投送 (F)")
    ("d" eaf-file-browser-qrcode " 隔空投送 (D)")
    ("i" eaf-open-airshare " 隔空投送 (S)")
    ("a" eaf-open-rss-reader "RSS 阅读器 "))))

(defhydra hydra-common-menu ()
  "
						^ 常用 ^
------------------------------------------------------
[_g_] ^counsel-rg^					[_y_] ^counsel-yank-pop^
[_f_] ^counsel-fzf^					[_d_] ^counsel-dired^
[_r_] ^counsel-recentf^				[_m_] ^counsel-bookmark^
[_b_] ^counsel-switch-buffer^		[_l_] ^counsel-linux-app^
"
  ("g" counsel-rg nil :color blue)
  ("f" counsel-fzf nil :color blue)
  ("r" counsel-recentf nil :color blue)
  ("b" counsel-switch-buffer nil :color blue)
  ("y" counsel-yank-pop nil :color blue)
  ("d" counsel-dired nil :color blue)
  ("m" counsel-bookmark nil :color blue)
  ("l" counsel-linux-app nil :color blue)
  ("q" nil "QUIT" :color blue))

(general-define-key
 :prefix "M-RET"
 "g" 'toggles-hydra/body
 "t" 'awesome-fast-switch/body
 "r" 'rect-hydra/body
 "o" 'origami-hydra/body
 "e" 'eaf-hydra/body
 "c" 'hydra-common-menu/body
 "w" 'hydra-window-menu/body)

(provide 'init-keybind)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keybind.el ends here
