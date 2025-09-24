;;; init-python.el --- Init Python -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Python language.

;;; Code:

(require 'init-emacs)
(require 'python)

(setq python-shell-interpreter "python")
(setq python-shell-interpreter-args "-m IPython --simple-prompt")

(keymap-set python-base-mode-map "C-c C-k" #'python-shell-send-buffer)

(dolist (mode '(python-mode python-ts-mode))
  (add-to-list 'init-evil-eval-function-alist `(,mode . python-shell-send-region)))

(provide 'init-python)
;;; init-python.el ends here
