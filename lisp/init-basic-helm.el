;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-core-utils)

(setq-declare! helm
  helm-ff-fuzzy-matching nil
  helm-locate-library-fuzzy-match nil
  helm-projectile-fuzzy-match nil
  helm-projectile-truncate-lines t
  helm-bookmark-show-location t
  helm-buffer-max-length 40
  helm-buffer-skip-remote-checking t)

(helm-mode 1)

(after-load! helm-mode
  (diminish! helm))

(declare-function! helm
  helm-resume)

(global-set-key! "<f5>" #'helm-resume)

(helm-projectile-on)

(helm-x-setup)

(provide 'init-basic-helm)
