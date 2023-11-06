;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-module))

(require 'init-core-vars)

(use-site-package!
 hy-python)

(use-elpa-package!
 hy-mode)

(provide 'init-lang-hy-pkg)
