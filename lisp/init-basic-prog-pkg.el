;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-module))

(require 'init-core-vars)

(use-elpa-package!
 projectile
 helm-projectile
 yasnippet
 company
 format-all)

(provide 'init-basic-prog-pkg)
