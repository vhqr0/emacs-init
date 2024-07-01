;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-emacs)

(defvar init-clojure-modes '(clojure-mode clojurescript-mode clojurec-mode))
(defvar init-clojure-mode-hooks '(clojure-mode-hook clojurescript-mode-hook clojurec-mode-hook))

(defvar clojure-mode-map)
(defvar cider-repl-mode-map)

(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map [remap format-all-region-or-buffer] #'cider-format-buffer))

(with-eval-after-load 'cider
  (helm-cider-mode 1)
  (define-key cider-repl-mode-map [remap helm-x-history] #'helm-cider-repl-history))

(dolist (hook init-clojure-mode-hooks)
  (add-hook hook #'init-enable-smartparens))

(dolist (mode init-clojure-modes)
  (add-to-list 'evil-x-eval-function-alist `(,mode . cider-eval-region)))

(provide 'init-clojure)
