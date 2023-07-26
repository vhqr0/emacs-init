;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-core-utils)

(setq-declare! smart-mode-line
  sml/theme 'respectful)

(sml/setup)

(after-load! projectile
  (diminish! projectile))

(provide 'init-ui-sml)
