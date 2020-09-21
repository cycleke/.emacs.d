;; init-base.el --- Better default configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;;
;; Better defaults.
;;

;;; Code:

(eval-when-compile
  (require 'init-variables)
  (require 'init-custom))

(setq user-full-name "cycleke")
(setq user-mail-address "cycleke@gmail.com")

(if (display-graphic-p)
    (progn
      (setenv "LANG" "en_US.UTF-8")
      (setenv "LC_ALL" "en_US.UTF-8")
      (setenv "LC_CTYPE" "en_US.UTF-8"))
  (progn
    (setenv "LANG" "zh_CN.UTF-8")
    (setenv "LANGUAGE" "zh-CN:en_US")
    (setenv "LC_ALL" "en_US.UTF-8")
    (setenv "LC_CTYPE" "en_US.UTF-8")))

;; 任何地方都使用 UTF-8
(set-charset-priority 'unicode)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq locale-coding-system 'utf-8
      default-process-coding-system '(utf-8-unix . utf-8-unix))
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-unix)
(setq confirm-kill-emacs
      (lambda (_) (y-or-n-p-with-timeout "Whether to quit Emacs? " 10 "n")))

;; 更友好及平滑的滚动
(setq scroll-step 2
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)

;; 关闭自动调节行高
(setq auto-window-vscroll nil)

(menu-bar-mode -1)
(tool-bar-mode -1)

;; file edit settings
(setq-default tab-width 2
              sentence-end-double-space nil
              make-backup-files nil
              indent-tabs-mode nil
              make-backup-files nil
              auto-save-default nil)

(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" user-cache-directory)
      eshell-history-file-name
      (expand-file-name "eshell/history" user-cache-directory))

;; (add-hook 'after-change-major-mode-hook (lambda ()
;;                                           (modify-syntax-entry ?_ "w")))
;; ;; "-" 同上)
;; (add-hook 'after-change-major-mode-hook (lambda ()
;;                                           (modify-syntax-entry ?- "w")))

(when (or sys/mac-x-p sys/linux-x-p)
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil
	        exec-path-from-shell-variables '("PATH" "MANPATH")
	        exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

(cond (sys/macp
       (progn
         ;; modify option and command key
         (setq mac-command-modifier 'super)
         (setq mac-option-modifier 'meta)
         ;; batter copy and paste support for mac os x
         (defun copy-from-osx ()
           (shell-command-to-string "pbpaste"))
         (defun paste-to-osx (text &optional push)
           (let ((process-connection-type nil))
             (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
               (process-send-string proc text)
               (process-send-eof proc))))
         (setq interprogram-cut-function 'paste-to-osx)
         (setq interprogram-paste-function 'copy-from-osx)

         (message "Wellcome To Mac OS X, Have A Nice Day!!!"))))

;; Start server
(use-package server
  :if (not sys/rootp)
  :commands server-running-p
  :config
  (unless (server-running-p)
    (server-start)))

;; History
(use-package saveplace
  :ensure t
  :hook (after-init . save-place-mode)
  :init
  (setq save-place-file (expand-file-name "places" user-cache-directory)))

(use-package recentf
  :ensure t
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
	            recentf-save-file (expand-file-name "recentf" user-cache-directory)
	            recentf-exclude
	            '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
		            "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
		            "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
		            "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
		            (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude))


(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
	            history-length 1000
	            savehist-file (expand-file-name ".savehist" user-cache-directory)
	            savehist-additional-variables '(mark-ring
					                                    global-mark-ring
					                                    search-ring
					                                    regexp-search-ring
					                                    extended-command-history)
	            savehist-autosave-interval 300))

(use-package time
  :ensure t
  :if (display-graphic-p)
  :hook (after-init . display-time-mode)
  :config
  (setq-default display-time-24hr-format t
	              display-time-day-and-date t)
  (display-time-mode 1))

(use-package simple
  :ensure nil
  :hook ((window-setup . size-indication-mode)
	       ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
	      line-number-mode t
	      ;; kill-whole-line t               ; Kill line including '\n'
	      line-move-visual nil
	      track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
	      set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace t) ; Show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

(use-package socks
  :ensure t
  :custom
  (url-gateway-method 'socks)
  (socks-server '("Default server" "localhost" 7891 5)))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

(push
 '(condition-case nil
      (progn
        (show-paren-mode 1)
        (scroll-bar-mode -1))
    (error nil))
 graphic-only-plugins-setting)
;; (toggle-scroll-bar -1)
(display-battery-mode 1)

(setq-default fill-column 80)
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell t
      inhibit-compacting-font-caches nil
      ring-bell-function 'ignore
      blink-cursor-mode nil)

(provide 'init-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-base.el ends here
