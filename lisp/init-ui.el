;;; init-ui.el --- UI 設置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;; 字體、主題等設置
;;
;;; Code:

(defconst lu-typeface-sample-text
  (concat
   "\n"
   "| More haste, less speed. |\n"
   "| 为天地立心，为生民立命；|\n"
   "| 𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄉𠄀。|\n"
   "\n"
   "|aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|\n"
   "|你你你你你你你你你你你你你你你你你你你你|\n"
   "|。。。。。。。。。。。。。。。。。。。。|\n"
   "|,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,|\n"
   "|1111111111111111111111111111111111111111|\n"
   "|東東東東東東東東東東東東東東東東東東東東|\n"
   "|ここここここここここここここここここここ|\n"
   "|ｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺ|\n"
   "|abcdefghijklmnopqrstABCDEFGHIJKLMNOPQRST|\n"
   "|uvxwyzUVWXYZ1234567890                  |\n"
   "|Il1iL10oOq9g!@#$%^&*()-=_+,./<>?{}'\":[]||\n")
  "字體測試樣本文本.")

(defun lu-message-sample-text ()
  "在 *Message* 緩衝區中顯示字體測試樣本文本."
  (interactive)
  (message "%s" lu-typeface-sample-text))

(defun lu-insert-sample-text ()
  "在當前位置插入字體測試樣本文本."
  (interactive)
  (insert lu-typeface-sample-text))

;; 字體設置參考 Lunarymacs
;; https://github.com/casouri/lunarymacs/blob/68db0d7e949b40e4829f7eb21f37973ddc532e56/site-lisp/lunary.el

(defun lu-font-installed-p (font)
  "確認字體 FONT 是否已安裝."
  (let ((spec (cond ((stringp font) (font-spec :name font))
                    ((fontp font) font)
                    (t nil))))
    (if (and spec (find-font spec))
        t
      (when (and font (not spec))
        (warn "無效的字體類型: %s" font))
      nil)))

(defconst lu-cjk-charsets
  '((#x4e00 . #x9fff) ;; CJK Unified Ideographs
    (#x3400 . #x4DBF) ;; CJK-ExtA
    (#xFF00 . #xFFEF) ;; 全角標點
    kana han cjk-misc bopomofo hangul)
  "CJK 相關字符集列表，用於設置 CJK 字體.")

(defvar lu-font-alist
  `(("MonaspaceNeon" "Monaspace Neon" "LXGW ZhiSong MN" 1.0)
    ("MonaspaceArgon" "Monaspace Argon" "LXGW ZhiSong MN" 1.0)
    ("MonaspaceXenon" "Monaspace Xenon" "LXGW ZhiSong MN" 1.0)
    ("MonaspaceRadon" "Monaspace Radon" "LXGW WenKai Screen" 1.0)
    ("MonaspaceKrypton" "Monaspace Krypton" "LXGW ZhiSong MN" 1.0)

    ("IBMPlex" "IBM Plex Mono" "LXGW ZhiSong MN" 1.0)

    ("Monolinear" "Helvetica" "LXGW XiHei CL" 1.0)
    ("ModulatedText" "Charter" "LXGW ZhiSong CL" 1.0)
    ("ModulatedTitle" "Charter" "LXGW WenKai TC" 1.0)

    ("MapleMonoNF" "Maple Mono NF CN" nil 1.0)
    ("LXGWBrightCode" "LXGW Bright Code" nil 1.0)
    ("LXGWWenKaiMono" "LXGW WenKai Mono Screen" nil 1.0))
  "字體配置列表.
每個元素格式為（其中 `CJK-NAME' 可以為 nil 或空字符串）

    (KEY ASCII-NAME CJK-NAME CJK-SCALE &rest EXTRAS)")

(defvar lu-default-charset-font-alist
  `((emoji
     . ,(if lu-is-mac
            "Apple Color Emoji"
          "Noto Color Emoji SVG"))
    (symbol
     . ,(if lu-is-mac
            "Apple Symbols"
          "Noto Sans Symbols"))
    (((#x20000 . #x2A6DF)   ; Ext-B
      (#x2A700 . #x2B73F)   ; Ext-C
      (#x2B740 . #x2B81F)   ; Ext-D
      (#x2B820 . #x2CEAF)   ; Ext-E
      (#x2CEB0 . #x2EBEF)   ; Ext-F
      (#x30000 . #x3134F))  ; Ext-G
     . (:font ("TH-Tshyn-P0" "TH-Tshyn-P1" "TH-Tshyn-P2" "TH-Tshyn-P16")
              :add 'prepend)))
  "可通過 `lu-load-charset-font` 切換的字體配置列表.
每個元素格式為

    (CHARSET . FONT)
    (CHARSET . (:font FONT :add ADD))")

(defun lu-create-fontset (ascii-spec cjk-spec)
  "創建一個基於 ASCII-SPEC 的 fontset，並為 CJK 字符應用 CJK-SPEC."
  (let ((ascii-family (plist-get ascii-spec :family))
        (ascii-font (and ascii-spec (apply #'font-spec ascii-spec))))
    (if (not (lu-font-installed-p ascii-font))
        (and (warn "ASCII 字體未安裝: %s" ascii-family) nil)
      (let* ((font-hash (sxhash (list ascii-spec cjk-spec)))
             (fontset-name
              (format "fontset-%s+%x"
                      (downcase ascii-family)
                      (abs font-hash)))
             (fontset
              (create-fontset-from-fontset-spec
               (font-xlfd-name
                (apply #'font-spec :registry fontset-name ascii-spec))))
             (cjk-family (and cjk-spec (plist-get cjk-spec :family)))
             (cjk-font (and cjk-family (apply #'font-spec cjk-spec))))

        ;; 只要 CJK 字體非空，就為 CJK 字符集單獨設置字體
        (when cjk-font
          (if (lu-font-installed-p cjk-font)
              (dolist (charset lu-cjk-charsets)
                (set-fontset-font fontset charset cjk-font))
            (warn "CJK 字體未安裝: %s" cjk-family)))
        fontset))))

(defun lu-font-expand-spec (font-spec size)
  "根據 FONT-SPEC 和 SIZE 生成 (ASCII-SPEC CJK-SPEC) 列表.
FONT-SPEC 格式為 (ASCII-NAME CJK-NAME CJK-SCALE &rest EXTRAS).
如果 ASCII 或 CJK 名稱為 nil 或空字符串，則對應的 SPEC 為 nil."
  (cl-destructuring-bind
      (raw-ascii-family raw-cjk-family cjk-scale &optional ascii-extra-spec cjk-extra-spec)
      font-spec
    (let* ((ascii-family
            (and (stringp raw-ascii-family)
                 (not (string-empty-p raw-ascii-family))
                 raw-ascii-family))
           (cjk-family
            (and (stringp raw-cjk-family)
                 (not (string-empty-p raw-cjk-family))
                 raw-cjk-family))
           (ascii-spec
            (when ascii-family
              `(:family ,ascii-family
                        ,@(when size `(:size ,size)) ,@ascii-extra-spec)))
           (cjk-spec
            (when cjk-family
              `(:family ,cjk-family
                        ,@(when (and size cjk-scale)
                            `(:size
                              ,(if (integerp size)
                                   (round (* cjk-scale size))
                                 (* cjk-scale size))))
                        ,@cjk-extra-spec))))
      (list ascii-spec cjk-spec))))

(defun lu-get-fontset (font-name &optional size)
  "獲取名稱為 FONT-NAME 的 fontset，字體大小為 SIZE (默認為 12)."
  (let ((size (or size 12)))
    (when-let* ((font-spec-entry (alist-get font-name lu-font-alist nil nil #'equal))
                (specs (lu-font-expand-spec font-spec-entry size)))
      (apply #'lu-create-fontset specs))))

(defun lu-load-default-font (font-spec size &rest attrs)
  "設置默認字體為 FONT-SPEC，大小為 SIZE，屬性為 ATTRS.
如果 ASCII 為 nil，則僅設置 CJK 字體.

注意：我們對默認字體使用單獨的函數，因為 Emacs 存在一個 bug，
阻止我們為默認 face 設置 fontset（儘管 'set-frame-parameter' 有效）。
因此我們只用 ASCII 字體設置默認 face，並使用默認 fontset 處理 Unicode 字體。"
  (interactive
   (let ((key (completing-read
               "Key: " (mapcar #'car lu-font-alist) nil t))
         (font-size (read-number "Size: " 12.0)))
     (list (alist-get key lu-font-alist nil nil #'equal)
           font-size)))
  (let* ((specs (lu-font-expand-spec font-spec size))
         (ascii-spec (car specs))
         (cjk-spec (cadr specs)))
    (when ascii-spec
      (let ((ascii-font (apply #'font-spec ascii-spec)))
        (if (lu-font-installed-p ascii-font)
            (apply #'set-face-attribute 'default nil :font ascii-font attrs)
          (warn "ASCII 字體未安裝: %s" (plist-get ascii-spec :family)))))
    (when cjk-spec
      (let ((cjk-font (apply #'font-spec cjk-spec)))
        (if (lu-font-installed-p cjk-font)
            (dolist (charset lu-cjk-charsets)
              (set-fontset-font t charset cjk-font))
          (warn "CJK 字體未安裝: %s" (plist-get cjk-spec :family)))))))

(defun lu-load-font (face font-name size &rest attrs)
  "為 FACE 加載名稱為 FONT-NAME 的字體，大小為 SIZE，支持附加屬性 ATTRS.
如果 FACE 為 'default，則使用 `lu-load-default-font` 的完整邏輯."
  (interactive
   (let ((face (completing-read
                "Face: " (face-list)))
         (key (completing-read
               "Key: " (mapcar #'car lu-font-alist) nil t))
         (font-size (read-number "Size: " 12.0)))
     (list (intern face) key font-size)))

  (let ((font-spec (alist-get font-name lu-font-alist nil nil #'equal)))
    (if (eq face 'default)
        (apply #'lu-load-default-font font-spec size attrs)
      (let ((fontset (apply #'lu-create-fontset (lu-font-expand-spec font-spec size))))
        (apply #'set-face-attribute face nil
               :font fontset
               :fontset fontset attrs)))))


(defun lu-load-charset-font (&optional fontset charset-font-alist)
  "為 FONTSET 設置特定的字符集字體，配置來自 CHARSET-FONT-ALIST."
  (let ((fontset (or fontset (frame-parameter nil 'font)))
        (alist-to-use (or charset-font-alist lu-default-charset-font-alist)))
    (dolist (entry alist-to-use)
      (let* ((charsets (car entry))
             (config (cdr entry))
             (fonts (or (plist-get config :font) config))
             (add-policy (plist-get config :add)))
        ;; 遍歷每個字體
        (dolist (font (if (listp fonts) fonts (list fonts)))
          (if (lu-font-installed-p font)
              (progn
                ;; 遍歷每個字符集
                (dolist (charset (if (listp charsets) charsets (list charsets)))
                  (set-fontset-font fontset charset (font-spec :family font) nil add-policy)))
            (warn "字符集字體未安裝: %s (字符集: %s)" font charsets)))))))

(defun lu-init-fonts (&rest _)
  "初始化當前會話的字體."
  (message "字體初始化完成【無實際操作】"))

(defvar lu-theme 'modus-operandi
  "選中的主題名稱，默認為 modus-operandi.")

(defun lu-init-theme (&rest _)
  "初始化主題."
  (interactive)
  (when lu-theme
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (if (featurep (intern (format "%s-theme" lu-theme)))
        ;; We can save a lot of time by only enabling the theme.
        (enable-theme lu-theme)
      (load-theme lu-theme t))
    (message "已初始化主題 %s" lu-theme)))

(let ((hook (if (daemonp)
                'server-after-make-frame-hook
              'after-init-hook)))
  (add-hook hook #'lu-init-theme -90)
  (add-hook hook #'lu-init-fonts -90))

(setq uniquify-buffer-name-style 'forward)

(setq frame-title-format
      '(:eval (concat (when (and buffer-file-name (buffer-modified-p))
                        "* ")
                      (buffer-name)
                      (when buffer-file-name
                        (format " (%s) - Emacs"
                                (directory-file-name (abbreviate-file-name default-directory))))))
      icon-title-format frame-title-format)

;; 設置縮放模式
(setq frame-resize-pixelwise t
      window-resize-pixelwise nil)

;; 滾動設置
(setq hscroll-step 1
      hscroll-margin 2
      scroll-margin 0
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
      fast-but-imprecise-scrolling t)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

;; 鼠標滾動
(setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2
      mouse-wheel-progressive-speed nil)

;; 不使用對話框
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; 窗口分割
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(setq split-width-threshold 160
      split-height-threshold nil)
(add-hook 'after-init-hook #'window-divider-mode)

(setq-default indicate-empty-lines t)
(unless indicate-empty-lines
  (toggle-indicate-empty-lines))

(use-package ediff
  :commands (ediff ediff-files)
  :custom
  (ediff-diff-options "-w")
  (ediff-split-window-function #'split-window-horizontally))

(use-package paren
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  ;; 添加中文括號匹配
  (modify-syntax-entry ?【 "(】" (standard-syntax-table))
  (modify-syntax-entry ?】 ")【" (standard-syntax-table))
  (modify-syntax-entry ?「 "(」" (standard-syntax-table))
  (modify-syntax-entry ?」 ")「" (standard-syntax-table))
  (modify-syntax-entry ?『 "(』" (standard-syntax-table))
  (modify-syntax-entry ?』 ")『" (standard-syntax-table))
  (modify-syntax-entry ?（ "(）" (standard-syntax-table))
  (modify-syntax-entry ?） ")（" (standard-syntax-table))
  (modify-syntax-entry ?《 "(》" (standard-syntax-table))
  (modify-syntax-entry ?》 ")《" (standard-syntax-table))
  (modify-syntax-entry ?〈 "(〉" (standard-syntax-table))
  (modify-syntax-entry ?〉 ")〈" (standard-syntax-table)))

(use-package whitespace
  :diminish
  :hook (prog-mode conf-mode text-mode)
  :custom
  (whitespace-line-column nil)
  (whitespace-style '(face trailing lines-char newline missing-newline-at-eof empty tab-mark newline-mark))
  (whitespace-display-mappings
   '((space-mark ?\s [?\u00B7] [46])
     (newline-mark ?\n [?\u00AC ?\n] [?$ ?\n])
     (tab-mark ?\t [?\u2192 ?\t] [?\u00BB ?\t] [?\\ ?\t]))))

(use-package display-line-numbers
  :hook (prog-mode text-mode conf-mode)
  :init
  (setq-default display-line-numbers-width 3
                display-line-numbers-widen t))

(use-package diminish
  :demand
  :config
  (dolist (mode '(subword-mode which-key-mode eldoc-mode))
    (diminish mode)))

(use-package auto-dark
  :diminish
  :custom (auto-dark-themes '((modus-vivendi) (modus-operandi)))
  :preface
  (defun on-auto-dark-dark ()
    (setq lu-theme (car (car auto-dark-themes))))
  (defun on-auto-dark-light ()
    (setq lu-theme (car (cadr auto-dark-themes))))
  :hook
  (auto-dark-dark-mode . on-auto-dark-dark)
  (auto-dark-light-mode . on-auto-dark-light))

;; 終端下忽略字體設置
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))
(unless (fboundp 'set-fontset-font)
  (fset 'set-fontset-font #'ignore))

;; 不使用 GTK+ 提示
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

(provide 'init-ui)
;;; init-ui.el ends here
