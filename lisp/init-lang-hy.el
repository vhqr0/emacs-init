;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(setq-declare! hy-mode
  hy-jedhy--enable? nil)

(after-load! hy-mode
  (require 'hy-python))

(add-hook! hy-mode paredit-mode)

(declare-variable! page-break-lines
  page-break-lines-modes)
(after-load! page-break-lines
  (add-to-list 'page-break-lines-modes 'hy-mode))

(declare-variable! evil-x
  evil-x-eval-function-alist)

(after-load! evil-x
  (add-to-list 'evil-x-eval-function-alist '(hy-mode . python-shell-send-region)))


(provide 'init-lang-hy)
