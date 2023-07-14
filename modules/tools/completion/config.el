;;; tools/completion/config.el --- 文本补全工具 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Lu Yaoke
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  文本补全工具
;;
;;; Code:

(use-package vertico
  :straight (:files (:defaults "extensions/*.el"))
  :hook (emacs-startup . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 17)
  :bind
  (:map
   vertico-map
   ("DEL" . vertico-directory-delete-char))
  :config
  ;; 补全时添加分隔提示符
  (defun crm-indicator (args)
    (cons
     (format
      "[CRM%s] %s"
      (replace-regexp-in-string
       "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'"
       ""
       crm-separator)
      (car args))
     (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; minibuffer 中不显示提示符
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; 不显示不支持的 vertico 指令
  (when (>= emacs-major-version 28)
    (setq read-extended-command-predicate
          #'command-completion-default-include-p))

  ;; 路径简化
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package orderless
  :init
  ;; Copied from doomemacs
  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))
  :custom
  (completion-styles '(substring orderless))
  (completion-category-overrides '((file (styles orderless partial-completion))))
  (orderless-style-dispatchers '(+vertico-orderless-dispatch))
  (orderless-component-separator "[ &]")
  :config
  (setq completion-category-defaults nil))

(use-package consult
  :bind
  ([remap bookmark-jump] . consult-bookmark)
  ([remap goto-line] . consult-goto-line)
  ([remap imenu] . consult-imenu)
  ([remap Info-search] . consult-info)
  ([remap locate] . consult-locate)
  ([remap load-theme] . consult-theme)
  ([remap man] . consult-man)
  ([remap recentf-open-files] . consult-recent-file)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap yank-pop] . consult-yank-pop)

  ("C-r" . consult-ripgrep)
  ("C-s" . consult-line)
  ("C-c i" . consult-imenu)
  ("C-x C-r" . consult-recent-file)

  :custom
  (consult-narrow-key "<")
  (consult-line-numbers-widen t)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1))

(use-package consult-dir
  :bind ([remap list-directory] . consult-dir))

(use-package marginalia
  :hook (emacs-startup . marginalia-mode))

;; 使用 Corfu 代替 Company
(use-package corfu
  :straight (:files (:defaults "extensions/*") :includes (corfu-popupinfo))
  :hook
  (emacs-startup . global-corfu-mode)
  (corfu-mode . corfu-popupinfo-mode)
  :bind
  (:map
   corfu-map
   ("<return>" . corfu-insert)
   ("<escape>" . corfu-quit)
   ([remap corfu-info-documentation] . corfu-popupinfo-toggle))
  :custom
  (completion-cycle-threshold nil)

  (corfu-cycle t)
  (corfu-auto t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-preview-current t)
  (corfu-preselect-first t)
  (corfu-echo-documentation t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)

  ;; corfu-popupinfo
  (corfu-doc-delay 0)
  (corfu-doc-max-width 70)
  (corfu-doc-max-height 20))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :commands corfu-terminal-mode
  :hook (corfu-mode . corfu-terminal-mode))

;;; tools/completion/config.el ends here
