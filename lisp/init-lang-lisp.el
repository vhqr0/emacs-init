;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-core-utils)

(comment! paredit
  (add-hook! lisp-data-mode paredit-mode)
  (after-load! paredit
    (diminish! paredit)))

(comment! macrostep
  (define-key! lisp-data-mode
    "C-c e" #'macrostep-expand))

(provide 'init-lang-lisp)
