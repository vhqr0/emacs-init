;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(setq inhibit-startup-screen t
      initial-scratch-message nil)

(setq use-dialog-box nil
      use-file-dialog nil)

(setq-default cursor-in-non-selected-windows nil)

(defvar!
 init--ui-disable-modes
 '(blink-cursor-mode tooltip-mode tool-bar-mode menu-bar-mode scroll-bar-mode))

(dolist (mode init--ui-disable-modes)
  (when (fboundp mode)
    (funcall mode -1)))

(setq tab-bar-tab-hints t
      tab-bar-select-tab-modifiers '(meta))

(global-set-key!
 "C-S-T" #'tab-bar-new-tab
 "C-S-W" #'tab-bar-close-tab)

(define-key! tab-prefix "`" #'toggle-frame-tab-bar)

(provide 'init-basic-ui)
