;;; -*- lexical-binding: t; no-native-compile: t -*-

(setq load-prefer-newer t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

(load-file (expand-file-name "emacs-init/lisp/init-core.el" user-emacs-directory))
(init-load)
