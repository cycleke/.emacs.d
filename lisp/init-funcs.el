;; init-funcs.el --- Define functions.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Define functions.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom)
  (require 'cl))

(defun open-init-file()
  "Open init.el file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))
(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
(defun indent-region-or-buffer ()
  "Indent a region of selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
	      (progn
	        (indent-region (region-beginning) (region-end))
	        (message "Indented selected region."))
      (progn
	      (indent-buffer)
	      (message "Indented the buffer.")))))
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer.")))
(global-set-key (kbd "s-r") #'revert-this-buffer)

(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

(defun recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

(defun recompile-site-lisp ()
  "Recompile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))

;; Pakcage archives
(defun set-package-archives (archives)
  "Set specific package ARCHIVES repository."
  (interactive
   (list
    (intern (completing-read
             "Choose package archives: "
             (mapcar #'car my-package-archives-alist)))))
  (customize-set-variable 'my-package-archives archives)
  (message "Set package archives to `%s'" archives))

;; Mode line
(defun mode-line-height ()
  "Get the height of the mode-line."
  (- (elt (window-pixel-edges) 3)
     (elt (window-inside-pixel-edges) 3)))

(defun compile-without-debug ()
  "Compile current buffer."
  (interactive)
  (let (filename suffix progname)
    (setq filename (file-name-nondirectory buffer-file-name))
    (setq progname (file-name-sans-extension filename))
    (setq suffix (file-name-extension filename))
    (if (string= suffix "c")
        (compile (concat "gcc " filename " -o " progname " -O2 -Wall -lm -std=gnu11 ")))
    (if (or (string= suffix "cc") (string= suffix "cpp"))
        (compile (concat "g++ " filename " -o " progname " -O2 -Wall -lm -std=gnu++14 ")))
    (if (string= suffix "java")
        (compile (concat "javac -encoding UTF-8 -sourcepath . -d . " filename)))))
(defun compile-with-debug ()
  "Compile current buffer."
  (interactive)
  (let (filename suffix progname)
    (setq filename (file-name-nondirectory buffer-file-name))
    (setq progname (file-name-sans-extension filename))
    (setq suffix (file-name-extension filename))
    (if (string= suffix "c")
        (compile (concat "gcc " filename " -o " progname " -g -Wall -lm -std=gnu11 ")))
    (if (or (string= suffix "cc") (string= suffix "cpp"))
        (compile (concat "g++ " filename " -o " progname " -g -Wall -lm -std=gnu++14 ")))))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %. ARG."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
	      ((looking-at "\\s)") (forward-char 1) (backward-list 1))
	      (t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)

;; LaTeX formatter
(defvar latex-format-binary "latexindent")
(defun latex-format-buffer ()
  "Use latexindent.pl to format the buffer."
  (interactive)
  (if (executable-find latex-format-binary)
      (progn
	      (shell-command
	       (concat latex-format-binary " -s "
		             "-cruft " user-cache-directory " "
		             buffer-file-name " -o " buffer-file-name " "
		             "-y=\"defaultIndent: ' ',maximumIndentation:' '\""))
	      (revert-buffer :ignore-auto :noconfirm))
    (error "%s" (concat latex-format-binary " not found."))))

(provide 'init-funcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-funcs.el ends here
