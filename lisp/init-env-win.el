;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'cl-macs))

(prefer-coding-system 'utf-8)

(add-to-list 'process-coding-system-alist '("rg" . (utf-8-dos . gbk-dos)))
(add-to-list 'process-coding-system-alist '("fd" . (utf-8-dos . gbk-dos)))

(defun init--around-coding-system-gbk (func &rest args)
  (let ((default-process-coding-system '(gbk-dos . gbk-dos)))
    (apply func args)))

(setq-declare! x-utils
  x-utils-xclip-program "clip.exe"
  x-utils-xclip-option ""
  x-utils-open-program "explorer.exe"
  x-utils-open-url-p t)

(add-advice! :around (x-utils-xclip-region x-utils-xdg-open) init--around-coding-system-gbk)

(setenv "GIT_ASKPASS" "git-gui--askpass")
(setenv "SSH_ASKPASS" "git-gui--askpass")

(provide 'init-env-win)
