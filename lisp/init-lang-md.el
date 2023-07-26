;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(setq-declare! markdown-mode
  markdown-fontify-code-blocks-natively t)

(provide 'init-lang-md)
