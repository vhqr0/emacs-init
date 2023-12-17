;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)
(require 'init-basic-prog)

(init-setq-declare!
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "--simple-prompt")

(defun init-python-fix-comment-inline-offset ()
  (setq-local comment-inline-offset 2))

(init-add-hook 'python-mode-hook #'init-python-fix-comment-inline-offset)

(add-to-list 'init-company-enabled-modes 'inferior-python-mode)

(defvar evil-x-eval-function-alist)

(with-eval-after-load 'evil-x
  (add-to-list 'evil-x-eval-function-alist '(python-mode    . python-shell-send-region))
  (add-to-list 'evil-x-eval-function-alist '(python-ts-mode . python-shell-send-region)))

(provide 'init-lang-py)
