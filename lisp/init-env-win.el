;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'cl-macs))

(prefer-coding-system 'utf-8)

(setcdr (assoc "" file-coding-system-alist) 'prefer-utf-8)

(setq-declare! x-utils
  x-utils-xclip-program "clip.exe"
  x-utils-xclip-option ""
  x-utils-open-program "explorer.exe"
  x-utils-open-url-p t)

(setenv "GIT_ASKPASS" "git-gui--askpass")
(setenv "SSH_ASKPASS" "git-gui--askpass")

(provide 'init-env-win)
