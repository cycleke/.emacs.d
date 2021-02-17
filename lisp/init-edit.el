;; init-edit.el --- Initialize editing configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Editing configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; Miscs
;; (setq initial-scratch-message nil)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Delete selection if you insert
(use-package delsel
  :ensure t
  :hook (after-init . delete-selection-mode))

;; Rectangle
(use-package rect+ :ensure t)

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure t
  :diminish
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t
	      auto-revert-verbose nil)
  (add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode))

;; Pass a URL to a WWW browser
(use-package browse-url
  :ensure nil
  :defines dired-mode-map
  :bind (("C-c C-z ." . browse-url-at-point)
         ("C-c C-z b" . browse-url-of-buffer)
         ("C-c C-z r" . browse-url-of-region)
         ("C-c C-z u" . browse-url)
         ("C-c C-z v" . browse-url-of-file))
  :init
  (with-eval-after-load 'dired
    (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map)))

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2))

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :diminish
  :commands drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; On-the-fly spell checker
(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
         ;; (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init
  (setq flyspell-issue-message-flag nil
        ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))


;; Treat undo history as a tree
;; FIXME:  keep the diff window
(make-variable-buffer-local 'undo-tree-visualizer-diff)
(use-package undo-tree
  :diminish
  :defines recentf-exclude
  :hook (after-init . global-undo-tree-mode)
  :init (setq undo-tree-visualizer-timestamps t
              undo-tree-visualizer-diff t
              undo-tree-enable-undo-in-region nil
              undo-tree-auto-save-history nil
              undo-tree-history-directory-alist
              `(("." . ,(expand-file-name "undo-tree-hist/" user-cache-directory))))
  :config (dolist (dir undo-tree-history-directory-alist)
            (push (expand-file-name (cdr dir)) recentf-exclude)))

;; Preview when `goto-line`
(use-package goto-line-preview
  :bind ([remap goto-line] . goto-line-preview))

;; Hideshow
(use-package hideshow
  :ensure t
  :diminish hs-minor-mode
  :bind (:map hs-minor-mode-map
	            ("C-c TAB" . hs-toggle-hiding)
              ("C-c p +" . hs-show-all))
  :hook (prog-mode . hs-minor-mode))

;; Flexible text folding
(use-package origami
  :hook (prog-mode . origami-mode)
  :init (setq origami-show-fold-header t)
  :config
  (face-spec-reset-face 'origami-fold-header-face)

  ;; Support LSP
  (use-package lsp-origami
    :hook (origami-mode . (lambda ()
			                      (if (bound-and-true-p lsp-mode)
				                        (lsp-origami-mode))))))

;; Open files as another user
(unless sys/win32p
  (use-package sudo-edit))

;; Narrow/Widen
(use-package fancy-narrow
  :diminish
  :hook (after-init . fancy-narrow-mode))

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; TODO, FIXME, etc highlighting in comments and strings
(use-package fic-mode
  :config
  (defun turn-on-fic-mode ()
    "turn fic-mode on"
    (interactive)
    (fic-mode 1))
  :hook
  ((prog-mode . turn-on-fic-mode)))

;; Switch window
(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :init
  (progn
    ;; (global-set-key [remap other-window] 'ace-window)
	  ;; 设置标记
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0 :foreground "magenta")))))))

(use-package windmove
  :ensure t
  :init (windmove-default-keybindings)
  :config
  :bind
  (:map leader-key
        ("w f" . #'windmove-right)
        ("w b" . #'windmove-left)
        ("w p" . #'windmove-up)
        ("w n" . #'windmove-down)
        ("w F" . #'window-move-right)
        ("w B" . #'window-move-left)
        ("w P" . #'window-move-up)
        ("w N" . #'window-move-down)
        ("w h" . #'enlarge-window-horizontally)
        ("w l" . #'shrink-window-horizontally)
        ("w j" . #'enlarge-window)
        ("w k" . #'shrink-window)))

;; 工作区
(use-package perspeen
  :disabled
  :diminish
  :ensure t
  :init
  ;; (setq perspeen-use-tab t)
  (setq perspeen-keymap-prefix [C-tab])
  (perspeen-mode))

(use-package howdoyou
  :ensure t
  :hook (after-init . howdoyou-mode))

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
