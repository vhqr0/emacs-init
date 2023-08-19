;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(setq default-input-method "pyim")

(global-set-key! "C-SPC" #'toggle-input-method)

(after-load! pyim
  (require 'init-app-pyim-al))

(provide 'init-app-pyim)
