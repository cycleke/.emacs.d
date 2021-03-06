;; init-latex.el --- Initialize latex configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Latex configuration.
;;

;;; Code:

(eval-when-compile
  (require 'init-variables)
  (require 'init-custom)
  (require 'init-company)
  (require 'init-funcs))

(use-package auctex
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (setq-default TeX-master nil)
  (setq TeX-auto-save t
	      TeX-parse-self t
	      TeX-syntactic-comment t
	      TeX-save-query nil
	      ;; Synctex support
	      TeX-source-correlate-start-server nil
	      ;; Don't insert line-break at inline math
	      LaTeX-fill-break-at-separators nil)

  (setq TeX-view-program-list
	      '(("SumatraPDF" "SumatraPDF.exe %o")
	        ("Gsview" "gsview32.exe %o")
	        ("Okular" "okular --unique %o")
	        ("Evince" "evince %o")
	        ("Firefox" "firefox %o")
	        ("zathura" "zathura %o")))
  (cond
   (sys/win32p
    (add-hook 'LaTeX-mode-hook
	            (lambda ()
		            (setq TeX-view-program-selection '((output-pdf "SumatraPDF")
						                                       (output-dvi "Yap"))))))
   (sys/linuxp
    (add-hook 'LaTeX-mode-hook
	            (lambda ()
		            (setq TeX-view-program-selection '((output-pdf "zathura")
						                                       (output-dvi "zathura"))))))
   (sys/linuxp
    (add-hook 'LaTeX-mode-hook
	            (lambda ()
		            (setq TeX-view-program-selection '((output-pdf "open")
						                                       (output-dvi "open"))))))
   )
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook 'prettify-symbols-mode))

(use-package company-math
  :ensure t
  :commands (luna-latex-company-setup)
  :init
  (add-hook
   'LaTeX-mode-hook
   (lambda ()
     (require 'company-math)
     (setq-local company-backends
		             (append '((company-math-symbols-latex company-latex-commands))
			                   company-backends)))))
(use-package cdlatex
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'cdlatex-mode)
  :config
  (general-unbind cdlatex-mode-map "TAB"))
(use-package auctex-latexmk
  :defer t
  :init (setq auctex-latexmk-inherit-TeX-PDF-mode t))
(use-package company-auctex
  :defer t)
(use-package company-reftex
  :defer t)

(provide 'init-tex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tex.el ends here
