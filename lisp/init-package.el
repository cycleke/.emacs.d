;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs Package management configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; Suppress warnings
(declare-function set-package-archives 'init-funcs)

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

;; Set ELPA packages
(set-package-archives my-package-archives)

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; A modern Packages Menu
(use-package paradox
  :init
  (setq paradox-execute-asynchronously t
	      paradox-github-token t
	      paradox-display-star-count nil)

  ;; Replace default `list-packages'
  (defun my-paradox-enable (&rest _)
    "Enable paradox, overriding the default package-menu."
    (paradox-enable))
  (advice-add #'list-packages :before #'my-paradox-enable)
  :config
  (when (fboundp 'page-break-lines-mode)
    (add-hook 'paradox-after-execute-functions
	            (lambda (&rest _)
		            (let ((buf (get-buffer-create "*Paradox Report*"))
		                  (inhibit-read-only t))
		              (with-current-buffer buf
		                (page-break-lines-mode 1))))
	            t)))

;; Auto update packages
(use-package auto-package-update
  :init
  (setq auto-package-update-delete-old-versions t
	      auto-package-update-hide-results t)
  (defalias 'upgrade-packages #'auto-package-update-now))

(use-package auto-compile
  :init (setq load-prefer-newer t)
  :config
  (progn
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

(use-package which-key
  :ensure t
  :custom
  (which-key-popup-type 'side-window)
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package format-all :defer t)
(use-package general :defer t)
(use-package try :ensure t)

(use-package hydra
  :ensure t
  :commands
  (hydra-default-pre
	 hydra-keyboard-quit
	 hydra--call-interactively-remap-maybe
	 hydra-show-hint
	 hydra-set-transient-map))

(use-package major-mode-hydra :ensure t :after hydra)

(use-package pretty-hydra
  :ensure t
  :defines (display-line-numbers-mode linum-mode)
  :functions set-package-archives
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
       (propertize title 'face face)))))

(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
