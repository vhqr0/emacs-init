;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-module))

(require 'init-core-vars)

(use-site-package!
 evil-x)

(use-elpa-package!
 evil
 evil-surround
 evil-collection)

(provide 'init-basic-evil-pkg)
