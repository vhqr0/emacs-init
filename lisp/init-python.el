;;; init-python --- Init Python -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Python language.

;;; Code:

(require 'init-emacs)

(setq! python-shell-interpreter "ipython")
(setq! python-shell-interpreter-args "--simple-prompt")

(defvar init-python-modes '(python-mode python-ts-mode inferior-python-mode))
(defvar init-python-mode-hooks '(python-base-mode-hook inferior-python-mode-hook))

(add-hook 'inferior-python-mode-hook #'python-mls-mode)

(defun init-python-fix-defaults ()
  "Fix local variables for `python-mode'."
  (setq-local
   comment-inline-offset 2
   forward-sexp-function nil))

(dolist (hook init-python-mode-hooks)
  (add-hook hook #'init-python-fix-defaults))

(dolist (hook init-python-mode-hooks)
  (add-hook hook #'flycheck-mode))

(dolist (mode init-python-modes)
  (add-to-list 'init-evil-eval-function-alist `(,mode . python-shell-send-region)))

(provide 'init-python)
;;; init-python.el ends here
