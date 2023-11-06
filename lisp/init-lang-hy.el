;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(setq-declare! hy-mode
  hy-jedhy--enable? nil)

(after-load! python-mode
  (require 'hy-python))

(after-load! hy-mode
  (require 'hy-python))

(add-hook! hy-mode paredit-mode)

(declare-variable! page-break-lines
  page-break-lines-modes)
(after-load! page-break-lines
  (add-to-list 'page-break-lines-modes 'hy-mode))

(provide 'init-lang-hy)
