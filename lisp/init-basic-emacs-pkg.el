;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-module))

(require 'init-core-vars)

(use-elpa-package!
 embark
 undo-tree
 page-break-lines
 rainbow-delimiters)

(provide 'init-basic-emacs-pkg)
