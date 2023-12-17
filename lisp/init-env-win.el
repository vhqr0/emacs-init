;;; -*- lexical-binding: t; no-native-compile: t -*-

(prefer-coding-system 'utf-8)

(setcdr (assoc "" file-coding-system-alist) 'prefer-utf-8)

(setenv "GIT_ASKPASS" "git-gui--askpass")
(setenv "SSH_ASKPASS" "git-gui--askpass")

(provide 'init-env-win)
