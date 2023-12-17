;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'cl-macs))

(require 'init-core-lib)

(init-setq-declare!
 recentf-exclude '("^/mnt/.*"))

(init-setq-declare!
 x-utils-xclip-program "clip.exe"
 x-utils-xclip-option ""
 x-utils-open-program (init-expand-misc-file-name "wsl-xdg-open.py"))

(init-setq-declare!
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
