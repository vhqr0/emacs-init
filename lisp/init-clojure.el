;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-emacs)

(defvar init-clojure-modes '(clojure-mode clojurescript-mode clojurec-mode))
(defvar init-clojure-mode-hooks '(clojure-mode-hook clojurescript-mode-hook clojurec-mode-hook))

(defvar clojure-mode-map)
(defvar cider-repl-mode-map)
(defvar clj-refactor-map)
(defvar cljr--all-helpers)

(declare-function helm-cider--override "helm-cider")

(with-eval-after-load 'cider
  (require 'helm-cider)
  (helm-cider--override)
  (define-key cider-repl-mode-map [remap helm-x-history] #'helm-cider-repl-history)
  (define-key clojure-mode-map [remap format-all-region-or-buffer] #'cider-format-buffer))

(with-eval-after-load 'clj-refactor
  (dolist (binding cljr--all-helpers)
    (evil-define-key '(motion normal visual operator) clj-refactor-map
      (concat "gr" (car binding)) (cadr binding)))
  (define-key clj-refactor-map [remap sp-splice-sexp-killing-around] #'cljr-raise-sexp)
  (define-key clj-refactor-map [remap sp-splice-sexp-killing-forward] #'cljr-splice-sexp-killing-forward)
  (define-key clj-refactor-map [remap sp-splice-sexp-killing-backward] #'cljr-splice-sexp-killing-backward))

(dolist (hook init-clojure-mode-hooks)
  (add-hook hook #'clj-refactor-mode)
  (add-hook hook #'init-enable-smartparens))

(dolist (mode init-clojure-modes)
  (add-to-list 'evil-x-eval-function-alist `(,mode . cider-eval-region)))

(provide 'init-clojure)
