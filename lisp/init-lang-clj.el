;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(declare-variable! clojure-mode
  clojure-mode-map)

(after-load! clojure-mode
  (define-key clojure-mode-map [remap format-all-region-or-buffer] #'cider-format-buffer))

(add-hook! (clojure-mode clojurescript-mode clojurec-mode) paredit-mode)

(declare-variable! page-break-lines
  page-break-lines-modes)
(after-load! page-break-lines
  (add-to-list 'page-break-lines-modes 'clojure-mode)
  (add-to-list 'page-break-lines-modes 'clojurescript-mode)
  (add-to-list 'page-break-lines-modes 'clojurec-mode))

(declare-variable! evil-x
  evil-x-eval-function-alist)

(after-load! evil-x
  (add-to-list 'evil-x-eval-function-alist '(clojure-mode       . cider-eval-region))
  (add-to-list 'evil-x-eval-function-alist '(clojurescript-mode . cider-eval-region))
  (add-to-list 'evil-x-eval-function-alist '(clojurec-mode      . cider-eval-region)))

(provide 'init-lang-clj)
