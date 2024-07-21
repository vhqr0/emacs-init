;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-emacs)

(defvar init-clojure-modes '(clojure-mode clojurescript-mode clojurec-mode cider-repl-mode))
(defvar init-clojure-mode-hooks '(clojure-mode-hook clojurescript-mode-hook clojurec-mode-hook cider-repl-mode-hook))

(defvar clojure-mode-map)
(defvar cider-repl-mode-map)
(defvar clj-refactor-map)
(defvar cljr--all-helpers)

(declare-function cider-doc "cider-doc")
(declare-function helm-cider--override "helm-cider")

(with-eval-after-load 'clojure-mode
  (require 'cider)
  (require 'clj-refactor)
  (require 'helm-cider)
  (helm-cider--override)
  (define-key cider-repl-mode-map [remap helm-x-history] #'helm-cider-repl-history)
  (define-key clojure-mode-map [remap format-all-region-or-buffer] #'cider-format-buffer)
  (dolist (binding cljr--all-helpers)
    (evil-define-key '(motion normal visual operator) clj-refactor-map
      (concat "gr" (car binding)) (cadr binding)))
  (define-key clj-refactor-map [remap sp-splice-sexp-killing-around] #'cljr-raise-sexp)
  (define-key clj-refactor-map [remap sp-splice-sexp-killing-forward] #'cljr-splice-sexp-killing-forward)
  (define-key clj-refactor-map [remap sp-splice-sexp-killing-backward] #'cljr-splice-sexp-killing-backward))

(defun init-lookup-setup-cider () (init-lookup-setup-command #'cider-doc))

(dolist (hook init-clojure-mode-hooks)
  (add-hook hook #'clj-refactor-mode)
  (add-hook hook #'init-enable-smartparens)
  (add-hook hook #'init-lookup-setup-cider))

(dolist (mode init-clojure-modes)
  (add-to-list 'init-evil-eval-function-alist `(,mode . cider-eval-region)))

(provide 'init-clojure)
