;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(setq text-quoting-style 'grave)

(xterm-mouse-mode 1)

(global-set-key! "C-M-_" #'dabbrev-completion)

(global-set-key! "C-@" #'toggle-input-method)

(declare-variable! evil
  evil-insert-state-map)

(after-load! evil
  (define-key! evil-insert-state "C-@" #'toggle-input-method))

(setq-declare! magit
  magit-section-visibility-indicator '("..." . t))

(provide 'init-env-term)
