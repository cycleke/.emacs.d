;; init-funcs.el --- Define functions.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Define functions.
;;

;;; Code:

(eval-when-compile
  (require 'init-variables)
  (require 'init-custom)
  (require 'cl-lib))

(defun open-init-dir ()
  "Open .emacs.d directory."
  (interactive)
  (dired user-emacs-directory))
(defun open-init-file ()
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
        (compile (concat "gcc " filename " -o " progname " -O2 -Wall -Wextra -Wshadow -lm -std=gnu11")))
    (if (or (string= suffix "cc") (string= suffix "cpp"))
        (compile (concat "g++ " filename " -o " progname " -O2 -Wall -Wextra -Wshadow -lm -std=gnu++14")))
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
        (compile (concat "gcc " filename " -o " progname " -g -Wall -Wextra -Wshadow -lm -std=gnu11")))
    (if (or (string= suffix "cc") (string= suffix "cpp"))
        (compile (concat "g++ " filename " -o " progname " -g -Wall -Wextra -Wshadow -lm -std=gnu++14")))))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert the ARG."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
	      ((looking-at "\\s)") (forward-char 1) (backward-list 1))
	      (t (self-insert-command (or arg 1)))))

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

(defun make-progress (width percent has-number?)
  "Make a progress bar with WIDTH and PERCENT.
HAS-NUMBER? tells whether to show the percent number."
  (let* ((done (/ percent 100.0))
         (done-width (floor (* width done))))
    (concat
     "["
     (make-string done-width ?/)
     (make-string (- width done-width) ? )
     "]"
     (if has-number? (concat " " (number-to-string percent) "%")))))

(defun insert-day-progress ()
  "Insert a day progress bar."
  (interactive)
  (let* ((today (time-to-day-in-year (current-time)))
         (percent (floor (* 100 (/ today 365.0)))))
    (insert (make-progress 30 percent t))))

(defun window-move (way)
  "移动窗口. WAY 是方向，可选值为 p,n,f,b，分别对应上下左右."
  (interactive "s 方向 (p-n-f-b): ")
  (let ((old-window-buffer (window-buffer))
        (old-window (get-buffer-window)))
    (pcase way ("p" (windmove-up))
           ("n" (windmove-down))
           ("f" (windmove-right))
           ("b" (windmove-left)))
    (let ((new-window-buffer (get-buffer-window)))
      (if (not (eql old-window-buffer new-window-buffer))
          (progn (set-window-buffer old-window (window-buffer))
                 (set-window-buffer (get-buffer-window) old-window-buffer))))))

(defun window-move-right ()
  "移动窗口到右方."
  (interactive)
  (let ((old-window-buffer (window-buffer))
        (old-window (get-buffer-window)))
    (if (windmove-right)
        (progn (set-window-buffer old-window (window-buffer))
               (set-window-buffer (get-buffer-window) old-window-buffer)))))

(defun window-move-left ()
  "移动窗口到左方."
  (interactive)
  (let ((old-window-buffer (window-buffer))
        (old-window (get-buffer-window)))
    (if (windmove-left)
        (progn (set-window-buffer old-window (window-buffer))
               (set-window-buffer (get-buffer-window) old-window-buffer)))))

(defun window-move-up ()
  "移动窗口到上方."
  (interactive)
  (let ((old-window-buffer (window-buffer))
        (old-window (get-buffer-window)))
    (if (windmove-up)
        (progn (set-window-buffer old-window (window-buffer))
               (set-window-buffer (get-buffer-window) old-window-buffer)))))

(defun window-move-down ()
  "移动窗口到下方."
  (interactive)
  (let ((old-window-buffer (window-buffer))
        (old-window (get-buffer-window)))
    (if (windmove-down)
        (progn (set-window-buffer old-window (window-buffer))
               (set-window-buffer (get-buffer-window) old-window-buffer)))))

(defun toggle-transparency ()
  "切换透明."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(90 . 90) '(100 . 100)))))

(defun toggle-proxy ()
  "切换代理."
  (interactive)
  (if (null url-proxy-services)
      (progn
        (setq url-proxy-services
              '(("http" . "127.0.0.1:7890")
                ("https" ."127.0.0.1:7890")))
        (message " 代理已开启."))
    (setq url-proxy-services nil)
    (message " 代理已关闭.")))

;; Haskell functions
(defun hlint-refactor-call-process-region-checked (start end program &optional args)
  "Send text from START to END to PROGRAM with ARGS.
This is a wrapper around `call-process-region' that doesn't replace
the region with the output of PROGRAM if it returned a non-zero
exit code."
  (let ((exit (apply 'call-process-region
                     start end
                     program            ; name of program
                     t                  ; delete region
                     t                  ; send output to buffer
                     nil                ; no redisplay during output
                     args
                     )))
    (unless (eq exit 0) (primitive-undo 1 buffer-undo-list))))


(defun hlint-refactor-call-process-region-preserve-point (start end program &optional args)
  "Send text from START to END to PROGRAM with ARGS preserving the point.
This uses `call-process-region-checked' internally."
  (let ((line (line-number-at-pos))
        (column (current-column)))
    (hlint-refactor-call-process-region-checked start end program args)
    (goto-line line)
    (move-to-column column)))

;;;###autoload
(defun hlint-refactor-refactor-buffer (&optional args)
  "Apply all hlint suggestions in the current buffer.
ARGS specifies additional arguments that are passed to hlint."
  (interactive)
  (hlint-refactor-call-process-region-preserve-point
   (point-min)
   (point-max)
   "hlint"
   (append '("--refactor" "-") args)))

;;;###autoload
(defun hlint-refactor-refactor-at-point ()
  "Apply the hlint suggestion at point."
  (interactive)
  (let ((col (number-to-string (+ 1 (current-column))))
        (line (number-to-string (line-number-at-pos))))
    (hlint-refactor-refactor-buffer
     (list (concat "--refactor-options=--pos " line "," col)))))

;;;###autoload
(define-minor-mode hlint-refactor-mode
  "Automatically apply hlint suggestions"
  :lighter " hlint-refactor"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "\C-cb" 'hlint-refactor-refactor-buffer)
            (define-key map "\C-cr" 'hlint-refactor-refactor-at-point)
            map))

(provide 'init-funcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-funcs.el ends here
