;; init-w3m.el --- Initialize w3m.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; w3m: A browser.
;;

;;; Code:

(use-package w3m
  :defer t
  :custom
  ;; W3M Home Page
  (w3m-home-page "https://start.duckduckgo.com/")

  ;; W3M default display images
  (w3m-default-display-inline-images t)
  (w3m-default-toggle-inline-images t)

  ;; W3M use cookies
  (w3m-command-arguments '("-cookie" "-F"))
  (w3m-use-cookies t)
  ;; Browse url function use w3m
  ;; (browse-url-browser-function 'w3m-browse-url)
  ;; W3M view url new session in background
  (w3m-view-this-url-new-session-in-background t)
  :init
  (setq browse-url-browser-function 'w3m-goto-url-new-session
	w3m-coding-system 'utf-8
	w3m-file-coding-system 'utf-8
	w3m-file-name-coding-system 'utf-8
	w3m-input-coding-system 'utf-8
	w3m-output-coding-system 'utf-8
	w3m-terminal-coding-system 'utf-8)
  :config
       (require 'w3m-search)
       (setq w3m-search-default-engine "duckduckgo")
       (add-to-list 'w3m-search-engine-alist
                    '("duckduckgo" "https://duckduckgo.com/lite/?q=%s")))

(defun toggle-env-http-proxy ()
  "Set/unset the environment variable http_proxy which w3m use."
  (interactive)
  (let ((proxy "http://127.0.0.1:1081"))
    (if (string= (getenv "http_proxy") proxy)
        ;; clear the proxy
        (progn
          (setenv "http_proxy" "")
          (message "env http_proxy is empty now"))
      ;; set the proxy
      (setenv "http_proxy" proxy)
      (message "env http_proxy is %s now" proxy))))

(provide 'init-w3m)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-w3m.el ends here
