;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-module))

(require 'init-core-vars)

(use-elpa-package!
 markdown-mode
 edit-indirect)

(provide 'init-lang-md-pkg)
