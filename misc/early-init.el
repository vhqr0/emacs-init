;;; -*- lexical-binding: t; no-native-compile: t -*-

(defvar early-init-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-percentage 0.8
      gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)

(defun early-init-after-init ()
  (setq gc-cons-percentage 0.2
        gc-cons-threshold (* 100 1000 1000)
        file-name-handler-alist
        (append early-init-file-name-handler-alist
                file-name-handler-alist)))

(add-hook 'after-init-hook #'early-init-after-init)
