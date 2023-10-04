;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-basic-prog)

(setq-declare! python
  python-shell-interpreter "ipython"
  python-shell-interpreter-args "--simple-prompt")

(defun-add-hook! python-mode
    init--python-fix-comment-inline-offset ()
  (setq-local comment-inline-offset 2))

(define-company-enabled-mode! inferior-python)

(add-hook! inferior-python-mode python-mls-mode)

(declare-variable! comint
  comint-last-prompt)

(defun-add-advice! :around python-shell-completion-at-point
                   init--save-comint-last-prompt-around-python-shell-completion-at-point (func &rest args)
  (let ((comint-last-prompt comint-last-prompt))
    (apply func args)))

(setq-declare! elpy
  elpy-remove-modeline-lighter nil
  elpy-modules
  '(elpy-module-company
    elpy-module-eldoc
    elpy-module-flymake
    elpy-module-pyvenv
    elpy-module-django))

(after-load! python
  (elpy-enable))

(declare-variable! elpy
  elpy-mode-map)

(declare-function! elpy
  elpy-shell-switch-to-shell)

(after-load! elpy
  (define-key! elpy-mode
    [remap python-shell-switch-to-shell] #'elpy-shell-switch-to-shell))

(provide 'init-lang-py)
