;;; init-completion.el --- 补全设置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;;  尽量使用 Emacs 自带的补全工具
;;
;;; Code:

(setq
 completion-auto-select t
 completion-auto-help 'always
 completions-format 'one-column
 completions-sort 'historical
 completion-show-help nil
 completion-show-inline-help nil
 completions-max-height 20
 completions-detailed t)

(add-to-list
 'display-buffer-alist
 '("\\*Completions\\*" (display-buffer-reuse-window display-buffer-in-side-window) (side . bottom) (slot . 0)))

(define-key minibuffer-local-completion-map (kbd "SPC") nil)
(define-key minibuffer-mode-map (kbd "C-n") #'minibuffer-next-completion)
(define-key minibuffer-mode-map (kbd "C-p") #'minibuffer-previous-completion)
(defun live-completions--update (&rest _)
  "Update the *Completions* buffer.
Meant to be added to `after-change-functions'."
  (when (minibufferp) ; skip if we've exited already
    (let ((while-no-input-ignore-events '(selection-request)))
      (while-no-input
        (condition-case nil
            (save-match-data
              (save-excursion
                (goto-char (point-max))
                (let ((inhibit-message t)
                      (ring-bell-function #'ignore))
                  (minibuffer-completion-help))))
          (quit (abort-recursive-edit)))))))
(defun live-completions--setup ()
  "Setup live updating for the *Completions* buffer.
Meant to be added to `minibuffer-setup-hook'."
  (unless (memq
           (or (bound-and-true-p current-minibuffer-command) this-command)
           '(execute-extended-command describe-command describe-symbol describe-function describe-variable))
    (add-hook 'after-change-functions #'live-completions--update nil t)))
(add-hook 'minibuffer-setup-hook #'live-completions--setup)

;; 开启补全预览
(add-hook 'prog-mode-hook #'completion-preview-mode)
(add-hook 'text-mode-hook #'completion-preview-mode)
(with-eval-after-load 'comint
  (add-hook 'comint-mode-hook #'completion-preview-mode))
(with-eval-after-load 'completion-preview
  (setq completion-preview-minimum-symbol-length 2)

  (push 'org-self-insert-command completion-preview-commands)
  (push 'paredit-backward-delete completion-preview-commands)

  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
  (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert))

(provide 'init-completion)
;;; init-completion.el ends here
