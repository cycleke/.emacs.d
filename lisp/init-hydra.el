;; init-hydra.el --- Initialize hydra configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Nice looking hydras.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package hydra
  :commands (hydra-default-pre
	     hydra-keyboard-quit
	     hydra--call-interactively-remap-maybe
	     hydra-show-hint
	     hydra-set-transient-map))

(use-package pretty-hydra
  :defines (display-line-numbers-mode linum-mode)
  :functions set-package-archives
  :bind ("M-RET g" . toggles-hydra/body)
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
				      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
	  (height (or height 1.0))
	  (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (display-graphic-p) icon-type icon-name)
	 (let ((f (intern (format "all-the-icons-%s" icon-type))))
	   (when (fboundp f)
	     (concat
	      (apply f (list icon-name :face face :height height :v-adjust v-adjust))
	      " "))))
       (propertize title 'face face))))

  ;; Global toggles
  (pretty-hydra-define toggles-hydra
    (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on")
					     :color amaranth :quit-key "q")
    ("Basic"
     (("n" (if (fboundp 'display-line-numbers-mode)
	       (display-line-numbers-mode (if display-line-numbers-mode -1 1))
	     (linum-mode (if linum-mode -1 1)))
       "line number" :toggle (if (fboundp 'display-line-numbers-mode)
				 display-line-numbers-mode
			       linum-mode))
      ("a" aggressive-indent-mode "aggressive indent" :toggle t)
      ("h" hungry-delete-mode "hungry delete" :toggle t)
      ("e" electric-pair-mode "electric pair" :toggle t)
      ("c" flyspell-mode "spell check" :toggle t)
      ("S" prettify-symbols-mode "pretty symbol" :toggle t)
      ("L" page-break-lines-mode "page break lines" :toggle t)
      ("M" doom-modeline-mode "modern mode-line" :toggle t))
     "Highlight"
     (("l" global-hl-line-mode "line" :toggle t)
      ("P" show-paren-mode "paren" :toggle t)
      ("s" symbol-overlay-mode "symbol" :toggle t)
      ("r" rainbow-mode "rainbow" :toggle t)
      ("w" (setq show-trailing-whitespace (not show-trailing-whitespace))
       "whitespace" :toggle show-trailing-whitespace)
      ("d" rainbow-delimiters-mode "delimiter" :toggle t)
      ("i" highlight-indent-guides-mode "indent" :toggle t)
      ("T" hl-todo-mode "todo" :toggle t))
     "Coding"
     (("f" flycheck-mode "flycheck" :toggle t)
      ("F" flymake-mode "flymake" :toggle t)
      ("o" origami-mode "folding" :toggle t)
      ("O" hs-minor-mode "hideshow" :toggle t)
      ("u" subword-mode "subword" :toggle t)
      ("W" which-function-mode "which function" :toggle t)
      ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
      ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))
     "Version Control"
     (("v" diff-hl-mode "gutter" :toggle t)
      ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
      ("m" diff-hl-margin-mode "margin gutter" :toggle t)
      ("D" diff-hl-dired-mode "dired gutter" :toggle t))
     "Package Archive"
     (("p m" (progn (setq my-package-archives 'melpa)
		    (set-package-archives my-package-archives))
       "melpa" :toggle (eq my-package-archives 'melpa))
      ("p i" (progn (setq my-package-archives 'melpa-mirror)
		    (set-package-archives my-package-archives))
       "melpa mirror" :toggle (eq my-package-archives 'melpa-mirror))
      ("p c" (progn (setq my-package-archives 'emacs-china)
		    (set-package-archives my-package-archives))
       "emacs china" :toggle (eq my-package-archives 'emacs-china))
      ("p n" (progn (setq my-package-archives 'netease)
		    (set-package-archives my-package-archives))
       "netease" :toggle (eq my-package-archives 'netease))
      ("p t" (progn (setq my-package-archives 'tencent)
		    (set-package-archives my-package-archives))
       "tencent" :toggle (eq my-package-archives 'tencent))
      ("p u" (progn (setq my-package-archives 'tuna)
		    (set-package-archives my-package-archives))
       "tuna" :toggle (eq my-package-archives 'tuna))))))
(provide 'init-hydra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-hydra.el ends here
