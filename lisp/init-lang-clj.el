;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)

(defvar clojure-mode-map)

(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map [remap format-all-region-or-buffer] #'cider-format-buffer))

(init-add-hook
 '(clojure-mode-hook clojurescript-mode-hook clojurec-mode-hook)
 (list #'smartparens-strict-mode #'evil-cleverparens-mode))

(defvar page-break-lines-modes)

(with-eval-after-load 'page-break-lines
  (add-to-list 'page-break-lines-modes 'clojure-mode)
  (add-to-list 'page-break-lines-modes 'clojurescript-mode)
  (add-to-list 'page-break-lines-modes 'clojurec-mode))

(defvar evil-x-eval-function-alist)

(with-eval-after-load 'evil-x
  (add-to-list 'evil-x-eval-function-alist '(clojure-mode       . cider-eval-region))
  (add-to-list 'evil-x-eval-function-alist '(clojurescript-mode . cider-eval-region))
  (add-to-list 'evil-x-eval-function-alist '(clojurec-mode      . cider-eval-region)))

(provide 'init-lang-clj)
