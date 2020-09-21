;; init-site-lisp.el --- Initialize other packages.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs Application FrameWork:
;;   EAF extends GNU Emacs to an entire universe of powerful GUI applications.
;;

;;; Code:

(eval-when-compile
  (require 'init-variables))

;; eaf
(push
 '(progn
    (require 'eaf)
    (use-package eaf
      :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
      :custom
      (eaf-find-alternate-file-in-dired t)
      :config
      (eaf-bind-key scroll_up "RET" eaf-pdf-viewer-keybinding)
      (eaf-bind-key scroll_down_page "DEL" eaf-pdf-viewer-keybinding)
      (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
      (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
      (eaf-bind-key take_photo "p" eaf-camera-keybinding)
      (eaf-bind-key undo_action "C-/" eaf-browser-keybinding)
      (eaf-bind-key redo_action "C-?" eaf-browser-keybinding)
      (eaf-bind-key scroll_up "M-j" eaf-browser-keybinding)
      (eaf-bind-key scroll_down "M-k" eaf-browser-keybinding)
      (eaf-bind-key scroll_up_page "M-n" eaf-browser-keybinding)
      (eaf-bind-key scroll_down_page "M-p" eaf-browser-keybinding)
      (eaf-bind-key scroll_to_begin "M->" eaf-browser-keybinding)
      (eaf-bind-key scroll_to_bottom "M-<" eaf-browser-keybinding)
      (eaf-bind-key open_link "M-h" eaf-browser-keybinding)
      (eaf-bind-key open_link_new_buffer "M-H" eaf-browser-keybinding)

      (add-hook 'circadian-after-load-theme-hook
                #'(lambda (_)
                    (if (and
                         (> (car (circadian-now-time)) (car (circadian-sunrise)))
                         (< (car (circadian-now-time)) (car (circadian-sunset))))
                        (progn
                          (eaf-setq eaf-pdf-dark-mode "false")
                          (eaf-setq eaf-browser-dark-mode "false")
                          (eaf-setq eaf-mindmap-dark-mode "false"))
                      (progn
                        (eaf-setq eaf-pdf-dark-mode "true")
                        (eaf-setq eaf-browser-dark-mode "true")
                        (eaf-setq eaf-mindmap-dark-mode "true")))))
      (setq eaf-config-location
            (file-name-as-directory (concat user-cache-directory "eaf")))
      (setq eaf-grip-token "0048eacd75ec58e1df586dfc95df732ebba3258e")
      (setq eaf-proxy-type "socks5")
      (setq eaf-proxy-host "127.0.0.1")
      (setq eaf-proxy-port "7891")
      (setq eaf-browser-default-search-engine "duckduckgo")
      (eaf-setq eaf-browser-aria2-proxy-host "127.0.0.1")
      (eaf-setq eaf-browser-aria2-proxy-port "7890")
      (eaf-setq eaf-browse-blank-page-url "https://duckduckgo.com"))

    (setq browse-url-browser-function 'eaf-open-browser)
    (defalias 'browse-web #'eaf-open-browser))
 graphic-only-plugins-setting)

;; fuz.el
(use-package fuz
  :load-path "~/.emacs.d/site-lisp/fuz.el"
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod)))
(require 'fuz)

;; snails
(push
 '(progn
    (require 'snails)
    (use-package snails
      :load-path "~/.emacs.d/site-lisp/snails"
      :bind ("C-c s" . snails)))
 graphic-only-plugins-setting)

(use-package company-english-helper
  :load-path "~/.emacs.d/site-lisp/company-english-helper"
  :bind (:map leader-key ("t h" . toggle-company-english-helper)))

;; (require 'unicad)

(use-package flywrap
  :disabled
  :load-path "~/.emacs.d/site-lisp/flywrap.el"
  :hook
  ((org-mode . flywrap-mode)
   (plain-TeX-mode . flywrap-mode)
   (plain-TeX-mode . flywrap-mode)
   (markdown-mode . flywrap-mode)
   (latex-mode . flywrap-mode)
   (LaTeX-mode . flywrap-mode)
   (tex-mode . flywrap-mode)))
;; (require 'flywrap)

(provide 'init-site-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-site-lisp.el ends here
