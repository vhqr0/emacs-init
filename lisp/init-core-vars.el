;;; -*- lexical-binding: t; no-native-compile: t -*-

(defconst init--cores
  '(core
    core-vars
    core-utils
    core-macs
    core-module))

(defvar init--modules
  '(basic-emacs
    basic-ui
    basic-edit
    basic-compl
    basic-evil
    basic-prog
    basic-tools
    basic-helm
    basic-maps

    ui-ef
    ui-sml
    ui-nano
    ui-fonts

    ;; env-term
    ;; env-wsl
    ;; env-cn

    lang-lisp
    lang-el
    lang-org
    lang-md
    lang-web
    lang-cc
    lang-py
    lang-hy))

(defvar init--elpa-archives
  '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"   )
    ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
    ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/" )))

(defvar init--site-packages nil)
(defvar init--elpa-packages nil)
(defvar init--core-paths nil)
(defvar init--module-paths nil)
(defvar init--module-al-paths nil)
(defvar init--module-pkg-paths nil)
(defvar init--site-paths nil)

(provide 'init-core-vars)
