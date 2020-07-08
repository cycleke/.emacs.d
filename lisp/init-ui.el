;; init-ui.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Visual (UI) configurations for better lookings and appearances.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-funcs))

(setq frame-title-format '("Emacs - %b")
      icon-title-format frame-title-format)

(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
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
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-major-mode-color-icon t
        doom-modeline-minor-modes nil
        doom-modeline-mu4e nil
        doom-modeline-height 1)
  (push
   '(custom-set-faces
     '(mode-line ((t (:family "Monaco" :height 0.9))))
     '(mode-line-inactive ((t (:family "Monaco" :height 0.9)))))
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
(use-package circadian
  :ensure t
  :init
  (setq calendar-latitude 31.47104)
  (setq calendar-longitude 104.73409)
  (setq circadian-themes '((:sunrise . spacemacs-light)
                           (:sunset  . gruvbox-dark-soft)))
  (circadian-setup)
  (add-hook 'circadian-after-load-theme-hook
            #'(lambda (_)
                ;; Cursor
                (set-default 'cursor-type 'box)
                ;; Line numbers appearance
                (setq linum-format 'linum-format-func))))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  ;; (set-face-foreground 'rainbow-delimiters-depth-1-face "orange red")
  ;; (set-face-foreground 'rainbow-delimiters-depth-2-face "gold")
  ;; (set-face-foreground 'rainbow-delimiters-depth-3-face "yellow")
  ;; (set-face-foreground 'rainbow-delimiters-depth-4-face "spring green")
  ;; (set-face-foreground 'rainbow-delimiters-depth-5-face "cyan")
  ;; (set-face-foreground 'rainbow-delimiters-depth-6-face "magenta")
  ;; (set-face-foreground 'rainbow-delimiters-depth-7-face "goldenrod")
  ;; (set-face-foreground 'rainbow-delimiters-depth-8-face "IndianRed1")
  ;; (set-face-foreground 'rainbow-delimiters-depth-9-face "ivory1")
  (set-face-bold 'rainbow-delimiters-depth-1-face "t")
  (set-face-bold 'rainbow-delimiters-depth-2-face "t")
  (set-face-bold 'rainbow-delimiters-depth-3-face "t")
  (set-face-bold 'rainbow-delimiters-depth-4-face "t")
  (set-face-bold 'rainbow-delimiters-depth-5-face "t")
  (set-face-bold 'rainbow-delimiters-depth-6-face "t")
  (set-face-bold 'rainbow-delimiters-depth-7-face "t")
  (set-face-bold 'rainbow-delimiters-depth-8-face "t")
  (set-face-bold 'rainbow-delimiters-depth-9-face "t"))

;; 切换buffer焦点时高亮动画
(use-package beacon
  :ensure t
  :hook (after-init . beacon-mode))

(use-package page-break-lines
  :ensure t
  :config (turn-on-page-break-lines-mode))

(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-items '((recents  . 10)
                        (projects . 5)))
  ;; 设置标题
  (setq dashboard-banner-logo-title
        (concat "Happy hacking, " user-login-name " - Emacs ♥ you!"))
  ;; 设置banner
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t))

;; Don't use GTK+ tooltip
(push
 '(progn
    (when (boundp 'x-gtk-use-system-tooltips)
      (setq x-gtk-use-system-tooltips nil))

    (defun my/zoom-in ()
      "Increase font size by 10 points."
      (interactive)
      (set-face-attribute 'default nil
                          :height
                          (+ (face-attribute 'default :height) 10)))

    (defun my/zoom-out ()
      "Decrease font size by 10 points."
      (interactive)
      (set-face-attribute 'default nil
                          :height
                          (- (face-attribute 'default :height) 10)))

    ;; change font size, interactively
    (global-set-key (kbd "C->") 'my/zoom-in)
    (global-set-key (kbd "C-<") 'my/zoom-out)

    ;; transparent
    (set-frame-parameter (selected-frame) 'alpha (list 95 90))
    (add-to-list 'default-frame-alist (cons 'alpha (list 95 90)))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

    (use-package all-the-icons
      :ensure t)

    ;; Font
    (use-package cnfonts
      :ensure t
      :config
      (setq cnfonts-profiles
            '("program" "writing"))
      (setq cnfonts-use-face-font-rescale t))
    (cnfonts-enable))
 graphic-only-plugins-setting)

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
