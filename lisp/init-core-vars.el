;;; -*- lexical-binding: t; no-native-compile: t -*-

(defconst init--cores
  '(core
    core-vars
    core-utils
    core-macs
    core-module))

(defvar init--modules
  '(basic-emacs
    basic-evil
    basic-helm
    basic-prog
    basic-tools
    basic-maps

    ui-themes
    ui-sml

    ;; env-win
    ;; env-wsl
    ;; env-term

    ;; cn-basic
    ;; cn-fonts
    ;; cn-pyim

    lang-lisp
    ;; lang-org
    ;; lang-md
    ;; lang-web
    ;; lang-py
    ;; lang-hy

    ;; app-roam
    ))

(defvar init--elpa-archives
  '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"   )
    ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
    ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/" )))

(defvar init--site-packages nil)
(defvar init--elpa-packages nil)
(defvar init--core-paths nil)
(defvar init--module-paths nil)
(defvar init--module-pkg-paths nil)
(defvar init--site-paths nil)

(provide 'init-core-vars)
