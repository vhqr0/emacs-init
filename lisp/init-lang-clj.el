;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)
(require 'init-basic-compl)

(defvar init-clojure-modes '(clojure-mode clojurescript-mode clojurec-mode))
(defvar init-clojure-mode-hooks '(clojure-mode-hook clojurescript-mode-hook clojurec-mode-hook))

(defvar clojure-mode-map)

(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map [remap format-all-region-or-buffer] #'cider-format-buffer))

(with-eval-after-load 'cider
  (helm-cider-mode 1)
  (define-key cider-repl-mode-map [remap helm-x-history] #'helm-cider-repl-history))

(init-add-hook
 init-clojure-mode-hooks
 (list #'smartparens-strict-mode #'evil-cleverparens-mode))

(add-to-list 'init-company-enabled-modes 'cider-repl-mode)

(defvar page-break-lines-modes)

(with-eval-after-load 'page-break-lines
  (init-append-to-list 'page-break-lines-modes init-clojure-modes))

(defvar evil-x-eval-function-alist)

(with-eval-after-load 'evil-x
  (init-append-to-list
   'evil-x-eval-function-alist
   (mapcar (lambda (mode) (cons mode 'cider-eval-region)) init-clojure-modes)))

(provide 'init-lang-clj)
