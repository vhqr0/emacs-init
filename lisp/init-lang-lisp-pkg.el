;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-module))

(require 'init-core-vars)

(use-elpa-package!
 paredit
 evil-cleverparens
 macrostep)

(provide 'init-lang-lisp-pkg)
