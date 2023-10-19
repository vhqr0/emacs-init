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
  hy--current-form-string)

(declare-function! easy-repl
  easy-repl-send-string)

(defun init--hy-shell-macroexpand-current-form ()
  (interactive)
  (require 'easy-repl)
  (easy-repl-send-string (format "(hy.macroexpand '%s)" (hy--current-form-string))))

(declare-variable! hy-mode
  hy-mode-map)

(after-load! hy-mode
  (define-key! hy-mode
    "C-c RET" #'init--hy-shell-macroexpand-current-form))

(declare-variable! page-break-lines
  page-break-lines-modes)
(after-load! page-break-lines
  (add-to-list 'page-break-lines-modes 'hy-mode))

(provide 'init-lang-hy)
