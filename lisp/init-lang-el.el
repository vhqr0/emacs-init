;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-basic-prog)

(define-key! (emacs-lisp-mode lisp-interaction-mode)
  "C-c e" #'macrostep-expand)

(set-eval-function! emacs-lisp eval-region)

(provide 'init-lang-el)
