;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(add-hook! after-init ef-themes-load-random)

(define-key! help "te" #'ef-themes-load-random)

(provide 'init-ui-ef)
