;;; -*- lexical-binding: t; no-native-compile: t -*-

(defvar!
 init--fonts-en "Iosevka"
 init-fonts-cjk "LXGW WenKai Mono")

(let ((font (font-spec :name init--fonts-en)))
  (when (find-font font)
    (set-face-attribute 'default nil :font font)))

(let ((font (font-spec :name init-fonts-cjk)))
  (dolist (chars '(cjk-misc han bopomofo kana hangul))
    (set-fontset-font t chars font)))

(provide 'init-ui-fonts)
