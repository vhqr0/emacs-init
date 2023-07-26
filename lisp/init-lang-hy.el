;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(after-load! hy-mode
  (require 'init-lang-hy-al))

(after-load! hy-shell
  (require 'hy-mode))

(provide 'init-lang-hy)
