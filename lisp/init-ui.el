;; init-ui.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Visual (UI) configurations for better lookings and appearances.
;;

;;; Code:

(eval-when-compile
  (require 'init-variables)
  (require 'init-funcs)
  (require 'cl-lib))

(setq frame-title-format '("" "%b - Emacs@" user-full-name)
      icon-title-format frame-title-format)
(set-default 'cursor-type 'box)

(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))

;; Menu/Tool/Scroll bars
(unless emacs/>=27p        ; Move to early init-file in 27
  (unless (display-graphic-p)
    (push '(menu-bar-lines . 0) default-frame-alist))
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist))

(use-package doom-modeline
  :disabled
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-major-mode-color-icon t
        doom-modeline-minor-modes nil
        doom-modeline-mu4e nil
        doom-modeline-height 1)
  (push
   '(custom-set-faces
     '(mode-line ((t (:family "Go Mono for Powerline" :height 0.72 :width condensed :weight light))))
     '(mode-line-inactive ((t (:family "Go Mono for Powerline" :height 0.72)))))
   graphic-only-plugins-setting))

;; Show native line numbers if possible, otherwise use linum
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :hook (prog-mode . display-line-numbers-mode))
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :init (setq linum-format "%4d ")
    :config
    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :custom-face (linum-highlight-face ((t (:inherit default :background nil :foreground nil))))
      :hook (global-linum-mode . hlinum-activate)
      :init (setq linum-highlight-in-all-buffersp t))))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-message nil)

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

(when (or sys/macp sys/linuxp)
  ;; Render thinner fonts
  (setq ns-use-thin-smoothing t)
  ;; Don't open a file in a new frame
  (setq ns-pop-up-frames nil))

(use-package spacemacs-theme :ensure :defer)
(use-package gruvbox-theme :ensure :defer)
(use-package leuven-theme :ensure :defer
  :init (load-theme 'leuven t))
(use-package tao-theme :ensure :defer)
(use-package doom-themes
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-opera-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
(use-package circadian
  :disabled :ensure t :init
  (setq calendar-latitude 45.75)
  (setq calendar-longitude 126.63)
  (setq circadian-themes '((:sunrise . doom-opera-light)
                           (:sunset  . doom-nord)))
  ;; (setq circadian-themes '(("8:00" . spacemacs-light)
  ;;                          ("18:00" . gruvbox-dark-soft)))
  (circadian-setup)
  (add-hook 'circadian-after-load-theme-hook
            #'(lambda (_)
                ;; Cursor
                (set-default 'cursor-type 'box)
                ;; Line numbers appearance
                (setq linum-format 'linum-format-func))))

;; 切换buffer焦点时高亮动画
(use-package beacon
  :ensure t
  :hook (after-init . beacon-mode)
  :custom
  (beacon-size 30)
  (beacon-color "cyan"))

(use-package page-break-lines
  :ensure t
  :hook (prog-mode . page-break-lines-mode)
  :config (page-break-lines-mode))

(use-package dashboard
  :ensure t :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-navigator t
        dashboard-set-footer t
        dashboard-week-agenda t

        dashboard-items '((recents  . 15)
                          (projects . 7)
                          (agenda . 5))
        dashboard-heading-icons '((recents   . "file-text")
                                  (bookmarks . "bookmark")
                                  (agenda    . "calendar")
                                  (projects  . "briefcase")
                                  (registers . "database"))
        dashboard-banner-logo-title (concat "Happy hacking, " user-login-name " - Emacs ♥ you!")))

(use-package info-colors
  :ensure t
  :hook ('Info-selection-hook . 'info-colors-fontify-node))

(use-package nyan-mode
  :disabled
  :ensure t
  :hook (after-init . nyan-mode))

;; Don't use GTK+ tooltip
(push
 '(progn
    (when (boundp 'x-gtk-use-system-tooltips)
      (setq x-gtk-use-system-tooltips nil))

    (defvar cycleke/zoom-count 0)

    (defun cycleke/zoom-in ()
      "Increase font size by 10 points."
      (interactive)
      (set-face-attribute
       'default nil
       :height
       (+ (face-attribute 'default :height) 10))
      (setq cycleke/zoom-count (1+ cycleke/zoom-count)))

    (defun cycleke/zoom-out ()
      "Decrease font size by 10 points."
      (interactive)
      (set-face-attribute
       'default nil
       :height
       (- (face-attribute 'default :height) 10))
      (setq cycleke/zoom-count (1- cycleke/zoom-count)))

    (defun cycleke/zoom-reset ()
      "Reset the font size."
      (interactive)
      (while (> cycleke/zoom-count 0)
        (cycleke/zoom-out))
      (while (< cycleke/zoom-count 0)
        (cycleke/zoom-in)))

    ;; change font size, interactively
    (general-define-key
     "C->" 'cycleke/zoom-in
     "C-<" 'cycleke/zoom-out
     "C-'" 'cycleke/zoom-reset)

    ;; transparent
    (set-frame-parameter (selected-frame) 'alpha (list 90 90))
    (add-to-list 'default-frame-alist (cons 'alpha (list 90 90)))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

    (use-package all-the-icons :ensure t)

		(use-package all-the-icons-dired
		  :ensure t
		  :hook ('dired-mode . 'all-the-icons-dired-mode))

    (use-package emojify
		  :after telega
		  :custom (emojify-emojis-dir (concat user-cache-directory "emojis"))
		  :config
		  (global-emojify-mode))

    (use-package posframe :disabled :ensure t)
		(use-package ivy-posframe
      :disabled
		  :ensure t
		  :init (ivy-posframe-mode 1)
		  :custom
      (ivy-posframe-parameters '((left-fringe . 8)
											           (right-fringe . 8)))
		  (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center))))

    (use-package hydra-posframe
      :disabled
      :load-path "~/.emacs.d/site-lisp/hydra-posframe"
      :hook (after-init . hydra-posframe-enable))


    ;; calculate the font size based on display-pixel-height
    (setq resolution-factor (eval (/ (x-display-pixel-height) 1080.0)))

    ;; Font
    (defun font-installed-p (font-name)
      "Check if font with FONT-NAME is available."
      (find-font (font-spec :name font-name)))

    (cl-loop for font in '("Sarasa Mono SC Nerd" "Fira Code" "SF Mono" "Hack" "Source Code Pro")
             when (font-installed-p font)
             return (set-face-attribute
                     'default nil
                     :font font
                     :height (round (* (cond (sys/mac-x-p 150)
                                             (sys/win32p 110)
                                             (t 110)))
                                    resolution-factor)))
    ;; Specify font for all unicode characters
    (cl-loop for font in '("Symbola" "Apple Symbols" "Symbol" "icons-in-terminal")
             when (font-installed-p font)
             return (set-fontset-font t 'unicode font nil 'prepend))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("Sarasa Mono SC Nerd" "Source Han Sans CN"
                           "FiraCode QiHei NF" "WenQuanYi Micro Hei" "Microsoft Yahei")
             when (font-installed-p font)
             return (set-fontset-font t '(#x4e00 . #x9fff) font)))
 graphic-only-plugins-setting)

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
