;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-module))

(require 'init-core-vars)

(use-site-package!
 prog-x)

(use-elpa-package!
 projectile
 yasnippet)

(provide 'init-basic-prog-pkg)
