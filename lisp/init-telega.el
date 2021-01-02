;; init-telega.el --- Initialize TELEGA.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Telega: GNU Emacs telegram client (unofficial)
;;

;;; Code:

(use-package telega
 :ensure t
 :commands telega
 :init
 (setq telega-proxies
       '((:server "localhost" :port 7891 :enable t
                  :type (:@type "proxyTypeSocks5"))))
 (setq telega-chat-fill-column 65)
 (setq telega-emoji-use-images nil)
 (set-fontset-font t 'unicode (font-spec :family "Symbola") nil 'prepend)
 (setq telega-symbol-unread "🄌")
 (with-eval-after-load 'company
   (add-hook 'telega-chat-mode-hook
             (lambda ()
               (make-local-variable 'company-backends)
               (dolist (it '(telega-company-botcmd telega-company-emoji))
                 (push it company-backends)))))
 (with-eval-after-load 'all-the-icons
   (add-to-list 'all-the-icons-mode-icon-alist
                '(telega-root-mode all-the-icons-fileicon "telegram"
                                   :heigt 1.0
                                   :v-adjust -0.2
                                   :face all-the-icons-yellow))
   (add-to-list 'all-the-icons-mode-icon-alist
                '(telega-chat-mode all-the-icons-fileicon "telegram"
                                   :heigt 1.0
                                   :v-adjust -0.2
                                   :face all-the-icons-blue)))
 (telega-notifications-mode 1)
 (telega-mode-line-mode 1))

(provide 'init-telega)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-telega.el ends here
