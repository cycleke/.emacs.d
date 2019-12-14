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


(setq-default major-mode 'text-mode)

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Rectangle
(use-package rect
  :ensure nil
  :bind (("<C-return>" . rect-hydra/body))
  :pretty-hydra
  ((:title (pretty-hydra-title "Rectangle" 'material "border_all" :height 1.1 :v-adjust -0.225)
	   :color amaranth :body-pre (rectangle-mark-mode) :post (deactivate-mark) :quit-key "q")
   ("Move"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→"))
    "Action"
    (("w" copy-rectangle-as-kill "copy") ; C-x r M-w
     ("y" yank-rectangle "yank")         ; C-x r y
     ("t" string-rectangle "string")     ; C-x r t
     ("d" kill-rectangle "kill")         ; C-x r d
     ("c" clear-rectangle "clear")       ; C-x r c
     ("o" open-rectangle "open"))        ; C-x r o
    "Misc"
    (("N" rectangle-number-lines "number lines")        ; C-x r N
     ("e" rectangle-exchange-point-and-mark "exchange") ; C-x C-x
     ("u" undo "undo")
     ("r" (if (region-active-p)
              (deactivate-mark)
            (rectangle-mark-mode 1))
      "reset")))))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
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

;; ;; Minor mode to aggressively keep your code always indented
;; (use-package aggressive-indent
;;   :diminish
;;   :hook ((after-init . global-aggressive-indent-mode)
;;          ;; FIXME: Disable in big files due to the performance issues
;;          ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
;;          (find-file . (lambda ()
;;                         (if (> (buffer-size) (* 3000 80))
;;                             (aggressive-indent-mode -1)))))
;;   :config
;;   ;; Disable in some modes
;;   (dolist (mode '(asm-mode web-mode html-mode css-mode go-mode prolog-inferior-mode))
;;     (push mode aggressive-indent-excluded-modes))

;;   ;; Disable in some commands
;;   (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

;;   ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
;;   (add-to-list
;;    'aggressive-indent-dont-indent-if
;;    '(and (or (derived-mode-p 'c-mode)
;;              (derived-mode-p 'c++-mode)
;;              (derived-mode-p 'csharp-mode)
;;              (derived-mode-p 'java-mode)
;;              (derived-mode-p 'go-mode)
;;              (derived-mode-p 'swift-mode))
;;          (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
;;                              (thing-at-point 'line))))))

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
  :ensure nil
  :diminish hs-minor-mode
  :bind (:map hs-minor-mode-map
	      ("C-`" . hs-toggle-hiding)))


;; Flexible text folding
(use-package origami
  :pretty-hydra
  ((:title (pretty-hydra-title "Origami" 'octicon "fold")
	   :color blue :quit-key "q")
   ("Node"
    ((":" origami-recursively-toggle-node "toggle recursively")
     ("a" origami-toggle-all-nodes "toggle all")
     ("t" origami-toggle-node "toggle current")
     ("o" origami-show-only-node "only show current"))
    "Actions"
    (("u" origami-undo "undo")
     ("d" origami-redo "redo")
     ("r" origami-reset "reset"))))
  :bind (:map origami-mode-map
	      ("C-`" . origami-hydra/body))
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
  (c-mode-common . turn-on-fic-mode)
  (prog-mode . turn-on-fic-mode))

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
