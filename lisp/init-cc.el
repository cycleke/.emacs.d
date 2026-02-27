;;; init-cc.el --- C/C++ 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023, Lu Yaoke. All rights reserved.
;; License: GPL v3, or (at your option) any later version
;;
;;; Commentary:
;;
;; C/C++ 配置
;;
;;; Code:

(use-package c-ts-mode
  :after treesit
  :custom
  (c-ts-mode-indent-offset 2)
  (c-ts-mode-indent-style 'k&r)
  :bind
  (:map c-ts-mode-map
   ("C-c C-c" . compile)
   :map c++-ts-mode-map
   ("C-c C-c" . compile))
  :preface
  (defun lu--setup-cc-ts ()
    (modify-syntax-entry ?_ "w")
    (treesit-font-lock-recompute-features)
    (when buffer-file-name
      (let* ((file (file-name-nondirectory buffer-file-name))
             (ext (file-name-extension file))
             (is-hdr (member ext '("h" "hpp" "hxx" "hh")))
             (is-cpp (member ext '("cpp" "cc" "cxx" "C" "c++")))
             (compiler (cond
                        ((and is-cpp (executable-find "clang++")) "clang++")
                        ((and is-cpp (executable-find "g++")) "g++")
                        ((executable-find "clang") "clang")
                        ((executable-find "gcc") "gcc")))
             (std (if is-cpp "-std=c++17" "-std=c11")))
        (setq-local compile-command
                    (cond
                     (is-hdr "make -k")
                     (compiler (format "%s -Wall -Wextra %s -g %s"
                                       compiler std file))
                     (t "make -k"))))))
  :hook ((c-ts-mode c++-ts-mode) . lu--setup-cc-ts))

(provide 'init-cc)
;;; init-cc.el ends here
