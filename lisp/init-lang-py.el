;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)
(require 'init-basic-compl)

(defvar init-python-modes '(python-mode python-ts-mode))

(init-setq-declare!
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "--simple-prompt")

(defun init-python-fix-comment-inline-offset ()
  (setq-local comment-inline-offset 2))

(init-add-hook 'python-mode-hook #'init-python-fix-comment-inline-offset)

(add-to-list 'init-company-enabled-modes 'inferior-python-mode)

(defvar evil-x-eval-function-alist)

(with-eval-after-load 'evil-x
  (init-append-to-list
   'evil-x-eval-function-alist
   (mapcar (lambda (mode) (cons mode 'python-shell-send-region)) init-python-modes)))

(provide 'init-lang-py)
