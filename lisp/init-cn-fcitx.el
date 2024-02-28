;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)

(setq fcitx-remote-command "fcitx5-remote")

(init-eval-after-init!
 (fcitx-aggressive-setup))

(provide 'init-cn-fcitx)
