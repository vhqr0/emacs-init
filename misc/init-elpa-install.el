;;; -*- lexical-binding: t; no-native-compile: t -*-

(setq load-prefer-newer t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (let ((backup-inhibited t))
    (load custom-file t t)))

(defvar init--directory      (expand-file-name "emacs-init" user-emacs-directory))
(defvar init--lisp-directory (expand-file-name "lisp"       init--directory))
(defvar init--site-directory (expand-file-name "site"       init--directory))
(defvar init--misc-directory (expand-file-name "misc"       init--directory))

(add-to-list 'load-path init--lisp-directory)
(add-to-list 'load-path init--site-directory)

(require 'init-core)

(init--module-load-for-meta)

(init--elpa-install)
