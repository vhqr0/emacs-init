;;; -*- lexical-binding: t; no-native-compile: t -*-

(defconst init-core-modules
  '(init-core
    init-core-vars
    init-core-lib
    init-core-meta
    init-core-module))

(defvar init-modules
  '(init-basic-emacs
    init-basic-evil
    init-basic-helm
    init-basic-prog
    init-basic-tools
    init-basic-maps

    init-ui-sml

    ;; init-env-win
    ;; init-env-wsl
    ;; init-env-term

    ;; init-cn-basic
    ;; init-cn-fonts
    ;; init-cn-pyim
    ;; init-cn-fcitx

    init-lang-lisp
    ;; init-lang-org
    ;; init-lang-md
    ;; init-lang-web
    ;; init-lang-py
    ;; init-lang-hy
    ;; init-lang-clj

    ;; init-app-roam
    ))

(defvar init-elpa-archives
  '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
    ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
    ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(provide 'init-core-vars)
