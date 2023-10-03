;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-module))

(require 'init-core-vars)

(use-elpa-package!
 projectile
 yasnippet
 format-all)

(provide 'init-basic-prog-pkg)
