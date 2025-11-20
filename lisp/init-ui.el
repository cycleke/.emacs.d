;;; init-ui.el --- UI 设置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  字体、主题等设置
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
   "|Il1iL10oOq9g!@#$%^&*()-=_+,./<>?{}'\":[]|\n")
  "Sample contents for typeface.")

(defun lu-message-sample-text ()
  "Show sample contents for typeface in *Message*."
  (interactive)
  (message "%s" lu-typeface-sample-text))

(defun lu-insert-sample-text ()
  "Insert sample contents for typeface."
  (interactive)
  (insert lu-typeface-sample-text))

;; 字体相关设置参考 Lunarymacs
;; https://github.com/casouri/lunarymacs/blob/68db0d7e949b40e4829f7eb21f37973ddc532e56/site-lisp/lunary.el

(defun lu-font-installed-p (font)
  "Check whether FONT is installed."
  (when-let* ((type (type-of font))
              (font-spec (cond
                          ((eq type 'string)
                           (font-spec :name font))
                          ((eq type 'font-spec)
                           font)
                          (t
                           nil))))
    (find-font font-spec)))

(defvar lu-font-alist
  `(
    ;; Coding
    ("IBMPlex" "IBM Plex Mono" "LXGW ZhiSong MN" 1.0)
    ("MonaspaceNeon" "Monaspace Neon" "LXGW ZhiSong MN" 1.0)

    ;; Handwritten
    ("MonaspaceRadon" "Monaspace Radon" "LXGW WenKai Screen" 1.0)

    ;; Variable
    ("ModulatedText" "Neuton" "LXGW ZhiSong CL" 1.0)
    ("ModulatedTitle" "Rufina" "LXGW WenKai TC" 1.0)
    ("Monolinear" "Helvetica Neue" "LXGW XiHei CL" 1.0)

    ;; Monospaced CJK
    ("MapleMono" "Maple Mono NF CN" "Maple Mono NF CN" 1.0)
    ("LXGWWenKaiMono" "LXGW WenKai Mono Screen" "LXGW WenKai Mono Screen" 1.0))
  "An alist of all the fonts you can switch between by `lu-load-font'.
Each element is like

    (FONT-NAME . (ASCII-NAME CJK-NAME CJK-SCALE ASCII-SPEC CJK-SPEC))")

(defvar lu-default-charset-font-alist
  `((emoji
     .
     ,(if lu-is-mac
          "Apple Color Emoji"
        "Noto Color Emoji SVG"))
    ((unicode symbol)
     .
     (:font ,(if lu-is-mac
                 "Apple Symbols"
               '("Noto Sans Symbols 2" "Noto Sans Symbols"))
            :add 'append))
    (((#x20000 . #x2fffff))
     .
     (:font ("TH-Tshyn-P0" "TH-Tshyn-P1" "TH-Tshyn-P2" "TH-Tshyn-P16") :add 'prepend))
    (nil
     .
     (:font ("Symbols Nerd Font Mono" "Noto Unicode") :add 'append)))
  "An alist of all the fonts you can switch between by `lu-load-charset-font'.
Each element is like

    (CHARSET . FONT)
    (CHARSET . (:font FONT :add ADD))")

(defun lu-create-fontset (ascii-spec cjk-spec)
  "Create a fontset NAME with ASCII-SPEC and CJK-SPEC font."
  (if (lu-font-installed-p (apply #'font-spec ascii-spec))
      (let* ((font-hash (sxhash (list (plist-get ascii-spec :family)
                                      (plist-get cjk-spec :family))))
             (fontset-name (format "fontset-%s+%x"
                                   (downcase (plist-get ascii-spec :family))
                                   (abs font-hash)))
             (fontset (create-fontset-from-fontset-spec
                       (font-xlfd-name
                        (apply #'font-spec :registry fontset-name ascii-spec)))))
        (if (lu-font-installed-p (apply #'font-spec cjk-spec))
            (dolist (charset '(kana han cjk-misc bopomofo (#x4e00 . #x9fff) hangul))
              (set-fontset-font fontset charset (apply #'font-spec cjk-spec)))
          (warn "FONT %s is not installed!!!" cjk-spec))
        fontset)
    (and (warn "FONT %s is not installed!!!" ascii-spec) nil)))

(defun lu-get-fontset (font-name)
  "Get fontset with FONT-NAME."
  (when-let* ((font-spec (alist-get font-name lu-font-alist nil nil #'equal))
              (fontset (apply #'lu-create-fontset (lu-font-expand-spec font-spec 1))))
    fontset))

(defun lu-font-expand-spec (font-spec size)
  "Translate FONT-SPEC, SIZE and ATTRS to (ASCII-SPEC CJK-SPEC)."
  (let* ((ascii-family (nth 0 font-spec))
         (cjk-family (nth 1 font-spec))
         (cjk-scale (nth 2 font-spec))
         (ascii-extra-spec (and size (append `(:size ,size) (nth 3 font-spec))))
         (cjk-extra-spec (and size
                              cjk-scale
                              (append `(:size ,(if (integerp size)
                                                   (round (* cjk-scale size))
                                                 (* cjk-scale size)))
                                      (nth 4 font-spec))))
         (ascii-spec (and ascii-family `(:family ,ascii-family ,@ascii-extra-spec)))
         (cjk-spec (and cjk-family `(:family ,cjk-family ,@cjk-extra-spec))))
    (list ascii-spec cjk-spec)))

(defun lu-load-default-font (font-spec size &rest attrs)
  "Set font for default face to FONT-SPEC with SIZE and ATTRS.
See `lu-load-font'."
  (interactive (let ((font-name (completing-read "FONT: " (mapcar #'car lu-font-alist) nil t))
                     (font-size (read-number "SIZE: " 10.0)))
                 (list (alist-get font-name lu-font-alist nil nil #'equal)
                       font-size)))
  (let* ((specs (lu-font-expand-spec font-spec size))
         (ascii (apply #'font-spec (car specs)))
         (cjk (apply #'font-spec (cadr specs))))
    (if (and ascii (lu-font-installed-p ascii))
        (apply #'set-face-attribute 'default nil :font ascii attrs)
      (warn "FONT %s is not installed!!!" ascii))
    (if (and cjk (lu-font-installed-p cjk))
        (dolist (charset '(kana han cjk-misc bopomofo (#x4e00 . #x9fff) hangul))
          (set-fontset-font t charset cjk))
      (warn "FONT %s is not installed!!!" ascii))))

(defun lu-load-font (face font-name size &rest attrs)
  "Load a FONT-SPEC for FACE with FONT-NAME size SIZE, additional ATTRS are supported."
  (interactive (list (intern (completing-read "FACE: " (face-list)))
                     (completing-read "FONT: " (mapcar #'car lu-font-alist) nil t)
                     (read-number "SIZE: " 10.0)))
  (let* ((font-spec (or (alist-get font-name lu-font-alist nil nil #'equal)
                        (list font-name "LXGW WenKai Screen" 1)))
         (fontset (apply #'lu-create-fontset (lu-font-expand-spec font-spec size))))
    (when fontset
      (if (eq face 'default)
          (apply #'lu-load-default-font font-spec size attrs)
        (apply #'set-face-attribute face nil
               :font fontset
               :fontset fontset attrs)))))

(defun lu-apply-to-list-wrapper (func vars)
  "Apply FUNC to each element in VARS."
  (if (and vars (listp vars))
      (mapcar func vars)
    (funcall func vars)))

(defun lu-load-charset-font (&optional fontset charset-font-alist)
  "Set font for specific charset CHARSET-FONT-ALIST to FONTSET."
  (let* ((fontset (or fontset (frame-parameter nil 'font)))
         (chatset-font-alist (or charset-font-alist lu-default-charset-font-alist)))
    (dolist (map chatset-font-alist)
      (let* ((charsets (car map))
             (configuration (cdr map))
             (fonts (or (plist-get configuration :font)
                        configuration))
             (add (plist-get configuration :add)))
        (lu-apply-to-list-wrapper
         (lambda (foNt)
           (if (lu-font-installed-p foNt)
               (lu-apply-to-list-wrapper
                (lambda (charset)
                  (set-fontset-font t charset (font-spec :family foNt) nil add))
                charsets)
             (warn "FONT %s is not installed!!!" foNt)))
         fonts)))))

(defun lu-set-buffer-font (font)
  "Set FONT for current buffer."
  (interactive "MFONT: ")
  (face-remap-set-base 'default `(:family ,font)))

(defvar lu--original-composition-table nil
  "Original `composition-function-table`.")

(defun lu-enable-ligatures ()
  "Enable ligatures for programming modes, saving original config if not saved."
  (unless lu--original-composition-table
    (setq lu--original-composition-table (copy-sequence composition-function-table)))
  (dolist (char/ligature-re
           `((?- . ,(rx (or "->" "-->")))
             (?/ . ,(rx (or (or "/==" "/=" "/>" "/**" "/*") (+ "/"))))
             (?< . ,(rx (or (or "<!--" "<-"
                                "</" "<="
                                "<>" "</>" "<|>"
                                "<=>" "<~>" "<->")
                            (+ "<"))))
             (?= . ,(rx (or (or "=/=" "=!=" "=>") (+ "="))))
             (?! . ,(rx (or (or "!==" "!=") (+ "!"))))
             (?> . ,(rx (or (or ">=") (+ ">"))))
             (?~ . ,(rx (or "~~>" "~~" "~>" "~-")))
             (?+ . ,(rx (+ "+")))
             (?\; . ,(rx (+ ";")))))
    (let ((char (car char/ligature-re))
          (ligature-re (cdr char/ligature-re)))
      (set-char-table-range composition-function-table char
                            `([,ligature-re 0 font-shape-gstring])))))

(defun lu-disable-ligatures ()
  "Restore original `composition-function-table` to disable ligatures."
  (when lu--original-composition-table
    (setq composition-function-table lu--original-composition-table)
    (setq lu--original-composition-table nil)))

(defun lu-init-fonts (&rest _)
  "Initalize fonts for current session."
  (message "Initalizing fonts, DO NOTHING"))

(defvar lu-theme 'modus-operandi
  "The selected theme namem, defaults to modus-operandi.")

(defun lu-init-theme (&rest _)
  "Initialize theme."
  (interactive)
  (when lu-theme
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (if (featurep (intern (format "%s-theme" lu-theme)))
        ;; We can save a lot of time by only enabling the theme.
        (enable-theme lu-theme)
      (load-theme lu-theme t))
    (message "Initalized theme %s." lu-theme)))

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
                        (format " (%s) - Emcas"
                                (directory-file-name (abbreviate-file-name default-directory))))))
      icon-title-format frame-title-format)

;; 设置缩放模式
(setq frame-resize-pixelwise t
      window-resize-pixelwise nil)

;; 滚动设置
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

;; 鼠标滚动
(setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2
      mouse-wheel-progressive-speed nil)

;; 不使用对话框
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
(when (not indicate-empty-lines)
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
  (show-paren-when-point-in-periphery t))

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

;; 终端下忽略字体设置
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))
(unless (fboundp 'set-fontset-font)
  (fset 'set-fontset-font #'ignore))

;; 不使用 GTK+ 提示
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

(provide 'init-ui)
;;; init-ui.el ends here
