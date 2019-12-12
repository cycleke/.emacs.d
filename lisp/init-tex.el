;; init-latex.el --- Initialize latex configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Latex configuration.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(use-package tex
  :ensure nil
  :init
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-syntactic-comment t
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
						   (output-dvi "zathura")))))))
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  :config
  (use-package auctex
    :defer t
    :ensure t)
  (use-package auctex-latexmk
    :defer t
    :init (setq auctex-latexmk-inherit-TeX-PDF-mode t))
  (use-package company-auctex
    :defer t
    :init (:backends
	   (company-auctex-macros
	    company-auctex-symbols
	    company-auctex-environments)
	   :modes LaTeX-mode))
  (use-package company-reftex
    :defer t
    :init (:backends
	   company-reftex-labels
	   company-reftex-citations
	   :modes LaTeX-mode))
  )


(provide 'init-tex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-latex.el ends here
