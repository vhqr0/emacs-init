;;; -*- lexical-binding: t; no-native-compile: t -*-

(setq exec-path
      (seq-remove
       (lambda (path) (string-prefix-p "/mnt/" path))
       exec-path))

(setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe")
(setq browse-url-generic-args '("/c" "start"))
(setq browse-url-browser-function 'browse-url-generic)
