;; init-org.el --- Initialize latex configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Org Mode configuration.
;;

;;; Code:

(require 'org-tempo)
(require 'org-capture)
(require 'ox-md)
(require 'ox-beamer)
(require 'ox-latex)

(use-package org
  :ensure t
  :bind
  ("C-c c" . 'org-capture)
  ("C-c a" . 'org-agenda)
  :custom
  (org-todo-keywords '((sequence "[学习](s!/@)" "[待办](t!/@)" "[等待](w!))" "|" "[完成](d!/@)" "[取消](c!@)")
                       (sequence "[BUG](b!/@)" "[新事件](i@)" "[已知问题](k!/@)" "[修改中](W!/@)" "|" "[已修复](f!)")))
  (org-todo-keyword-faces '(("[学习]" . (:foreground "white" :background "#2ECC71" :weight bold))
                            ("[待办]" . (:foreground "white" :background "#F1C40F" :weight bold))
                            ("[等待]" . (:foreground "white" :background "#3498DB" :weight bold))
                            ("[完成]" . (:foreground "white" :background "#566573" :weight bold))
                            ("[取消]" . (:foreground "white" :background "#566573" :weight bold))
                            ("[BUG]" . (:foreground "white" :background "#E74C3C" :weight bold))
                            ("[新事件]" . (:foreground "white" :background "#D35400" :weight bold))
                            ("[已知问题]" . (:foreground "white" :background "#17A589" :weight bold))
                            ("[修改中]" . (:foreground "white" :background "#BB8FCE" :weight bold))
                            ("[已修复]" . (:foreground "white" :background "#566573" :weight bold))))
  :config
  (setq org-capture-templates nil)
  ;; (setq org-time-stamp-formats '("<% Y-% m-% d 周 % u % H:% M>"))
  (add-to-list 'org-capture-templates
               '("t" " 任务清单 "))
  (add-to-list 'org-capture-templates '("tw" "工作任务" entry (file+headline "~/Documents/org/capture/task.org" "Work")
                                        "* [待办] %^{任务名} - %U\n  %a\n  %?" :clock-in t :clock-keep t))
  (add-to-list 'org-capture-templates '("ts" "学习任务" entry (file+headline "~/Documents/org/capture/task.org" "Study")
                                        "* [学习] %^{学习项目} - %U\n  %a\n  %?" :clock-in t :clock-keep t))
  (add-to-list 'org-capture-templates '("j" "我的日志" entry (file+headline"~/Documents/org/diary.org" "日志")
                                        "* %U - %^{标题}\n  %?"))
  (add-to-list 'org-capture-templates '("i" "我的闪念" entry (file+headline "~/Documents/org/idea.org" "闪念")
                                        "* %U - %^{标题} %^g\n  %?\n"))
  (add-to-list 'org-capture-templates '("k" "我的百科" entry (file+headline "~/Documents/org/wiki.org" "WIKI")
                                        "* %^{标题} %t %^g\n  %?\n"))
  (add-to-list 'org-capture-templates '("w" "我的单词" table-line (file+headline "~/Documents/org/capture/word.org" "Words")
                                        " | %U | %^{en_US} | %^{词性} | %^{zh_CN} |"))
  (add-to-list 'org-capture-templates '("l" "超链接" entry (file+headline "~/Documents/org/capture/link.org" "Links")
                                        "* %^{简介} %t %^g\n  %^L\n  %?\n"))

  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-preview-latex-process-alist
        '((dvisvgm
           :programs ("xelatex" "dvisvgm")
           :description "xdv > svg"
           :message "you need to install the programs: xelatex and dvisvgm."
           :use-xcolor t
           :image-input-type "xdv"
           :image-output-type "svg"
           :image-size-adjust (1.7 . 1.5)
           :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
           :image-converter("dvisvgm %f -n -b min -c %S -o %O"))

          (imagemagick
           :programs ("xelatex" "convert")
           :description "pdf > png"
           :message "you need to install the programs: xelatex and imagemagick."
           :use-xcolor t
           :image-input-type "pdf"
           :image-output-type "png"
           :image-size-adjust (1.0 . 1.0)
           :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
           :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))))
  ;; 代码高亮
  (setq org-src-fontify-natively t)
  ;; 导出为 HTML设置
  (setq org-html-doctype "html5")
  (setq org-html-xml-declaration nil)
  (setq org-html-postamble nil)
  (setq org-html-head
        "<link rel='stylesheet' href='http://cdn.bootcss.com/bootstrap/3.3.0/css/bootstrap.min.css'>
<link rel='stylesheet' href='http://cdn.bootcss.com/bootstrap/3.3.0/css/bootstrap-theme.min.css'>
<script src='http://cdn.bootcss.com/jquery/1.11.1/jquery.min.js'>
</script><script src='http://cdn.bootcss.com/bootstrap/3.3.0/js/bootstrap.min.js'></script>")

  (setcar (nthcdr 1 org-emphasis-regexp-components)
          "-[:space:].。,，:；!！?？;；'\")}\\")
  (org-set-emph-re 'org-emphasis-regexp-components
                   org-emphasis-regexp-components))

(use-package org-special-block-extras
  :ensure t
  :hook (org-mode . org-special-block-extras-mode))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-hide-leading-stars nil)
  (org-superstar-leading-bullet ?\s)
  (org-superstar-special-todo-items t))

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
