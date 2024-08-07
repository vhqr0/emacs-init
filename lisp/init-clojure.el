;;; init-clojure --- Init Clojure -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Clojure language.

;;; Code:

(require 'init-emacs)

(require 'clojure-mode)

(defvar init-clojure-modes '(clojurec-mode clojure-mode clojurescript-mode))
(defvar init-clojure-mode-hooks '(clojurec-mode-hook clojure-mode-hook clojurescript-mode-hook))

(require 'cider)
(require 'helm-cider)

(helm-cider--override)
(define-key cider-repl-mode-map [remap init-history-placeholder] #'helm-cider-repl-history)

(defun init-lookup-setup-cider () "Setup cider doc." (init-lookup-setup-command #'cider-doc))

(dolist (hook '(cider-mode-hook cider-repl-mode-hook))
  (add-hook hook #'init-lookup-setup-cider))

(require 'flycheck-clj-kondo)

(dolist (hook init-clojure-mode-hooks)
  (add-hook hook #'flycheck-mode))

(dolist (mode init-clojure-modes)
  (add-to-list 'init-evil-eval-function-alist `(,mode . cider-eval-region)))

(provide 'init-clojure)
;;; init-clojure.el ends here
