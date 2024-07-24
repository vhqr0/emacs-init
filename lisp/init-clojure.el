;;; init-clojure --- Init Clojure -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Clojure language.

;;; Code:

(require 'init-emacs)

(require 'clojure-mode)
(require 'cider)
(require 'helm-cider)
(require 'clj-refactor)
(require 'flycheck-clj-kondo)

(defvar init-clojure-modes '(clojure-mode clojurescript-mode clojurec-mode cider-repl-mode))
(defvar init-clojure-mode-hooks '(clojure-mode-hook clojurescript-mode-hook clojurec-mode-hook cider-repl-mode-hook))

(defun init-lookup-setup-cider () "Setup cider doc." (init-lookup-setup-command #'cider-doc))

(dolist (hook init-clojure-mode-hooks)
  (add-hook hook #'clj-refactor-mode)
  (add-hook hook #'init-enable-smartparens)
  (add-hook hook #'init-lookup-setup-cider)
  (when (not (eq hook 'cider-repl-mode-hook))
    (add-hook hook #'flycheck-mode)))

(helm-cider--override)
(define-key cider-repl-mode-map [remap init-history-placeholder] #'helm-cider-repl-history)

(dolist (binding cljr--all-helpers)
  (evil-define-key '(motion normal visual operator) clj-refactor-map
    (concat "gr" (car binding)) (cadr binding)))

(define-key clj-refactor-map [remap sp-splice-sexp-killing-around] #'cljr-raise-sexp)
(define-key clj-refactor-map [remap sp-splice-sexp-killing-forward] #'cljr-splice-sexp-killing-forward)
(define-key clj-refactor-map [remap sp-splice-sexp-killing-backward] #'cljr-splice-sexp-killing-backward)

(dolist (mode init-clojure-modes)
  (add-to-list 'init-evil-eval-function-alist `(,mode . cider-eval-region)))

;; Waiting PR: https://github.com/lassik/emacs-format-all-the-code/pull/257

(require 'format-all)

(define-format-all-formatter cider
  (:executable)
  (:install)
  (:languages "Clojure")
  (:features region)
  (:format
   (format-all--buffer-native
    'clojurec-mode
    (if region
        (lambda () (cider-format-region (car region) (cdr region)))
      (lambda () (cider-format-region (point-min) (point-max)))))))

(provide 'init-clojure)
;;; init-clojure.el ends here
