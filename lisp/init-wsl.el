;;; init-wsl.el --- Init WSL -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Windows Subsystem Linux.

;;; Code:

(require 'init-emacs)

(defun init-wsl-clean-windows-path ()
  "Clean windows path."
  (let ((path (->> (getenv "PATH")
                   (s-split ":")
                   (--remove (s-starts-with? "/mnt/" it))
                   (s-join ":"))))
    (setenv "PATH" path)))

(init-wsl-clean-windows-path)

(setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe")
(setq browse-url-generic-args '("/c" "start"))
(setq browse-url-browser-function 'browse-url-generic)

(provide 'init-wsl)
;;; init-wsl.el ends here
