;;; -*- lexical-binding: t; no-native-compile: t -*-

(setq load-prefer-newer t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defvar init--directory      (expand-file-name "emacs-init" user-emacs-directory))
(defvar init--lisp-directory (expand-file-name "lisp"       init--directory))
(defvar init--site-directory (expand-file-name "site"       init--directory))
(defvar init--misc-directory (expand-file-name "misc"       init--directory))

(add-to-list 'load-path init--lisp-directory)
(add-to-list 'load-path init--site-directory)

(require 'site-autoloads)
(require 'init-core)

(package-initialize)

(init--module-load)

(provide 'init)
