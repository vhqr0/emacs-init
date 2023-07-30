;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-module))

(require 'init-core-vars)

(use-site-package!
 helm-roam)

(use-elpa-package!
 org-roam
 org-roam-ui)

(provide 'init-app-roam-pkg)
