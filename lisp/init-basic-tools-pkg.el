;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-module))

(require 'init-core-vars)

(use-site-package!
 x-utils
 eshell-x)

(use-elpa-package!
 wgrep
 wgrep-helm
 rg
 magit
 with-editor)

(provide 'init-basic-tools-pkg)
