;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-module))

(require 'init-core-vars)

(use-elpa-package!
 undo-tree
 rainbow-delimiters)

(provide 'init-basic-emacs-pkg)
