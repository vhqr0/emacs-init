;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-basic-compl)
(require 'init-basic-prog)

(require 'python)

(defvar!
 init--ipython-program "ipython"
 init--ipython-option  "--simple-prompt")

(setq python-shell-interpreter      init--ipython-program
      python-shell-interpreter-args init--ipython-option)

(set-eval-function! python python-shell-send-region)

(defun-add-hook! python-mode
    init--python-fix-comment-inline-offset ()
  (setq-local comment-inline-offset 2))

(defun-add-advice! :around python-shell-completion-at-point
                   init--save-comint-last-prompt-around-python-shell-completion-at-point (func &rest args)
  (let ((comint-last-prompt comint-last-prompt))
    (apply func args)))

(define-company-enabled-mode! inferior-python)

(add-hook! inferior-python-mode python-mls-mode)

(require 'elpy)

(setq elpy-remove-modeline-lighter nil
      elpy-modules
      '(elpy-module-company
        elpy-module-eldoc
        elpy-module-flymake
        elpy-module-pyvenv
        elpy-module-django))

(elpy-enable)

(define-key! elpy-mode
  [remap python-shell-switch-to-shell] #'elpy-shell-switch-to-shell)

(provide 'init-lang-py-al)
