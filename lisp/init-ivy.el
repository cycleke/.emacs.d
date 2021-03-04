;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Ivy configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom)
  (require 'init-variables))

;; 增强了搜索功能
(use-package swiper
  :ensure t
  :bind
  (("C-s" . swiper)
   ("C-r" . counsel-rg)
   ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file))
  :config
  (setq swiper-action-recenter t)
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package counsel
  :ensure t
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :bind
  (("C-x C-r" . 'counsel-recentf)
   ("C-x d" . 'counsel-dired)
   :map swiper-map
   ("M-s" . swiper-isearch-toggle)
   ("M-%" . swiper-query-replace)
   :map isearch-mode-map
   ("M-s" . swiper-isearch-toggle)
   :map counsel-mode-map
   ([remap swiper] . counsel-grep-or-swiper)
   ([remap swiper-backward] . counsel-grep-or-swiper-backward)
   ([remap cd] . counsel-cd)
   ([remap dired] . counsel-dired)
   ([remap set-variable] . counsel-set-variable))
  :config
  (setq ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
        ivy-height 10
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil)
  ;; 默认的 rg 配置
  ;; (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s")
  (setq counsel-rg-base-command
        (list "rg" "-M" "240" "--with-filename" "--no-heading" "--line-number" "--color" "never" "%s" "-g" "!package-config.org" "-g" "!site-lisp"))
  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy)))

;; Enhance M-x
(use-package amx
  :ensure t
  :init (setq amx-history-length 100
              amx-save-file (expand-file-name "amx-items" user-cache-directory)))

;; 强大的字符跳转工具
(use-package avy
  :ensure t
  :bind (("M-g :" . 'avy-goto-char)
         ("M-g '" . 'avy-goto-char-2)
         ("M-g \"" . 'avy-goto-char-timer)
         ("M-g f" . 'avy-goto-line)
         ("M-g w" . 'avy-goto-word-1)
         ("M-g e" . 'avy-goto-word-0)))

(use-package ivy-rich
	:ensure t
	:init
  (ivy-rich-mode 1)
	(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
	:config
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
					(:columns ((ivy-rich-switch-buffer-icon (:width 2))
										 (ivy-rich-candidate (:width 30))
										 (ivy-rich-switch-buffer-size (:width 7))
										 (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
										 (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
										 (ivy-rich-switch-buffer-project (:width 15 :face success))
										 (ivy-rich-switch-buffer-path
                      (:width (lambda (x)
																(ivy-rich-switch-buffer-shorten-path
																 x
																 (ivy-rich-minibuffer-width
																	0.3))))))
                    :predicate (lambda (cand)
														     (get-buffer cand)))
					counsel-find-file
					(:columns ((ivy-read-file-transformer)
										 (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
					counsel-M-x
					(:columns ((counsel-M-x-transformer (:width 40))
										 (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))) ; return docstring of the command
					counsel-recentf
					(:columns ((ivy-rich-candidate (:width 0.8))
										 (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))) ; return last modified time of the file
					counsel-describe-function
					(:columns
					 ((counsel-describe-function-transformer (:width 40))
						(ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))) ; return docstring of the function
					counsel-describe-variable
					(:columns
					 ((counsel-describe-variable-transformer (:width 40))
						(ivy-rich-counsel-variable-docstring (:face font-lock-doc-face)))) ; return docstring of the variable
					)))

;; 美化 ivy (swiper 和 counsel)
(push
 '(progn
		;;美化
		(use-package all-the-icons-ivy-rich
		  :ensure t
		  :init
      (all-the-icons-ivy-rich-mode 1)))
 graphic-only-plugins-setting)

;; counsel提供对项目管理的支持
(use-package counsel-projectile
  :ensure t
  :hook ((counsel-mode . counsel-projectile-mode))
  :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
  :bind (:map leader-key ("p" . #'projectile-command-map)))

;;;###autoload
(defun ivy-telega-chat-highlight (chat)
  (let ((unread (funcall (telega--tl-prop :unread_count) chat))
        (title (telega-chat-title chat 'with-identity))
        (not-muted-p (not (telega-chat-muted-p chat)))
        (mentions (funcall (telega--tl-prop :unread_mention_count) chat)))

    (if (and not-muted-p (> (+ unread mentions) 0))
        (ivy-append-face (format "%s %d@%d" title unread mentions) 'ivy-highlight-face)
      title)))

;;;###autoload
(defun ivy-telega-chat-with ()
  "Starts chat with defined peer."
  (interactive)
  (telega t)
  (let ((chats (mapcar
                (lambda (x) (cons (ivy-telega-chat-highlight x) x))
                (telega-filter-chats telega--ordered-chats 'all))))
    (ivy-read "chat: " chats
              :action (lambda (x) (telega-chat--pop-to-buffer (cdr x)))
              :caller 'ivy-telega-chat-with)))
(bind-key "t c" #'ivy-telega-chat-with leader-key)
(setq telega-completing-read-function 'ivy-completing-read)

(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
