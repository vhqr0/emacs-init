;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-basic-compl)

(after-load! cc-mode
  (require 'init-lang-cc-al))

(after-load! cmake-mode
  (require 'init-lang-cc-al))

(provide 'init-lang-cc)
