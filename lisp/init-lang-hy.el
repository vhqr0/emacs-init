;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(setq-declare! hy-mode
  hy-jedhy--enable? nil
  hy-shell--interpreter-args nil)

(add-hook! hy-mode paredit-mode)

(after-load! hy-shell
  (require 'hy-mode))

(declare-function! hy-mode
  hy--current-form-string
  hy-shell--eval-1)

(defun init--hy-shell-macroexpand-current-form ()
  (interactive)
  (hy-shell--eval-1
    (format "(hy.macroexpand '%s)" (hy--current-form-string))))

(declare-variable! hy-mode
  hy-mode-map)

(after-load! hy-mode
  (define-key! hy-mode
    "C-c RET" #'init--hy-shell-macroexpand-current-form))

(provide 'init-lang-hy)
