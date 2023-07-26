;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'cl-macs)
  (require 'init-core-macs))

(require 'init-core-utils)

(setq-declare! recentf
  recentf-exclude '("^/mnt/.*"))

(setq-declare! x-utils
  x-utils-xclip-program "clip.exe"
  x-utils-xclip-option ""
  x-utils-open-program (init--expand-misc-file-name "wsl-xdg-open.py"))

(setq-declare! browse-url
  browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
  browse-url-generic-args '("/c" "start")
  browse-url-browser-function #'browse-url-generic)

(declare-function! eshell
  eshell/export)

(defun-add-hook! eshell-mode init--wsl-eshell-cleanup-path ()
  (interactive)
  (let ((paths (cl-loop for path in (split-string (getenv "PATH") ":")
                        when (not (string-prefix-p "/mnt/" path))
                        collect path)))
    (eshell/export (concat "PATH=" (string-join paths ":")))))

(provide 'init-env-wsl)
