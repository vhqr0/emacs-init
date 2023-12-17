;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)

(init-setq-declare!
 sml/theme 'respectful)

(init-eval-after-init!
 (sml/setup))

(with-eval-after-load 'projectile
  (init-diminish-minor-mode 'projectile-mode))

(provide 'init-ui-sml)
