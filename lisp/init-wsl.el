;;; init-wsl.el --- Init WSL -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Windows Subsystem Linux.

;;; Code:

(require 'init-emacs)

(setq exec-path
      (seq-remove
       (lambda (path) (string-prefix-p "/mnt/" path))
       exec-path))

(setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe")
(setq browse-url-generic-args '("/c" "start"))
(setq browse-url-browser-function 'browse-url-generic)

(provide 'init-wsl)
;;; init-wsl.el ends here
