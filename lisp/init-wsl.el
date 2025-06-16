;;; init-wsl.el --- Init WSL -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Windows Subsystem Linux.

;;; Code:

(require 'init-emacs)

(setq exec-path (->> exec-path (--remove (string-prefix-p "/mnt/" it))))

(setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe")
(setq browse-url-generic-args '("/c" "start"))
(setq browse-url-browser-function 'browse-url-generic)

(provide 'init-wsl)
;;; init-wsl.el ends here
