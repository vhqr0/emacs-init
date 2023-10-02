;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'cl-macs))

(add-to-list 'process-coding-system-alist '("rg" . (utf-8-dos . gbk-dos)))
(add-to-list 'process-coding-system-alist '("fd" . (utf-8-dos . gbk-dos)))

(defun init--around-coding-system-gbk (func &rest args)
  (let ((default-process-coding-system '(gbk-dos . gbk-dos)))
    (apply func args)))

(add-advice! :around (x-utils-xclip-region x-utils-xdg-open) init--around-coding-system-gbk)

(provide 'init-cn-win)
