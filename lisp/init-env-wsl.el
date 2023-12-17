;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'cl-macs))

(require 'init-core-lib)

(init-setq-declare!
 recentf-exclude '("^/mnt/.*"))

(defun init-xdg-open-file-wsl (file)
  (call-process-shell-command
   (format "%s %s"
           (init-expand-misc-file-name "wsl-xdg-open.py")
           (expand-file-name file))))

(init-setq-declare!
 xdg-open-file-function #'init-xdg-open-file-wsl
 browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
 browse-url-generic-args '("/c" "start")
 browse-url-browser-function #'browse-url-generic)

(declare-function eshell/export "eshell")

(defun init-wsl-eshell-cleanup-path ()
  (interactive)
  (let ((paths (cl-loop for path in (split-string (getenv "PATH") ":")
                        when (not (string-prefix-p "/mnt/" path))
                        collect path)))
    (eshell/export (concat "PATH=" (string-join paths ":")))))

(init-add-hook 'eshell-mode-hook #'init-wsl-eshell-cleanup-path)

(provide 'init-env-wsl)
