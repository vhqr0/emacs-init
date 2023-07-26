;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-module))

(require 'init-core-vars)

(use-site-package!
 sdcv)

(use-elpa-package!
 posframe
 popon
 pyim
 pyim-basedict)

(provide 'init-env-cn-pkg)
