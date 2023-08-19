;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-module))

(require 'init-core-vars)

(use-elpa-package!
 posframe
 popon
 pyim
 pyim-basedict)

(provide 'init-app-pyim-pkg)
