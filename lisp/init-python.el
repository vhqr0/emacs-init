;;; init-python.el --- Init Python -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Python language.

;;; Code:

(require 'init-emacs)
(require 'python)

(defvar init-python-modes '(python-mode python-ts-mode inferior-python-mode))
(defvar init-python-mode-hooks '(python-base-mode-hook inferior-python-mode-hook))

(setq python-shell-interpreter "python")
(setq python-shell-interpreter-args "-m IPython --simple-prompt")

(add-hook 'inferior-python-mode-hook #'init-corfu-set-auto)

(dolist (mode init-python-modes)
  (add-to-list 'init-evil-eval-function-alist `(,mode . python-shell-send-region)))

(provide 'init-python)
;;; init-python.el ends here
