;;; init-wsl.el --- Init WSL -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Windows Subsystem Linux.

;;; Code:

(require 'init-emacs)

(setq! browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe")
(setq! browse-url-generic-args '("/c" "start"))
(setq! browse-url-browser-function 'browse-url-generic)

(defun init-wsl-eshell-remove-windows-path ()
  "Remove windows path."
  (interactive)
  (let* ((path (-> (getenv "PATH") (split-string ":")))
         (path (->> path (--remove (string-prefix-p "/mnt/" it))))
         (path (-> path (string-join ":"))))
    (eshell/export (concat "PATH=" path))))

(add-hook 'eshell-mode-hook #'init-wsl-eshell-remove-windows-path)

(provide 'init-wsl)
;;; init-wsl.el ends here
