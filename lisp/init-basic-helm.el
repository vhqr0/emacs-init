;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-core-utils)

(helm-mode 1)

(diminish! helm)

(declare-function! helm
  helm-resume)

(global-set-key! "<f5>" #'helm-resume)

(helm-projectile-on)

(helm-x-setup)

(provide 'init-basic-helm)
