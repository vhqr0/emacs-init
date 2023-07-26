;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-module))

(require 'init-core-vars)

(use-site-package!
 simple-x)

(use-elpa-package!
 undo-tree)

(provide 'init-basic-emacs-pkg)
