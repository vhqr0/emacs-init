;;; init-clojure --- Init Clojure -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Clojure language.

;;; Code:

(require 'init-emacs)

;;; major mode

(require 'clojure-mode)

(defvar init-clojure-modes '(clojurec-mode clojure-mode clojurescript-mode))

(dolist (mode init-clojure-modes)
  (add-to-list 'init-evil-eval-function-alist `(,mode . cider-eval-region)))

;;; flymake

(require 'flymake-kondor)

(add-hook 'clojure-mode-hook #'flymake-kondor-setup)
(add-hook 'clojure-mode-hook #'flymake-mode)

;;; cider

(setq! cider-mode-line '(:eval (format " Cider[%s]" (cider--modeline-info))))

(require 'cider)
(require 'cider-format)
(require 'cider-macroexpansion)

(defun init-cider-set-lookup-command ()
  "Set lookup command for Cider."
  (setq-local evil-lookup-func #'cider-doc))

(dolist (hook '(cider-mode-hook cider-repl-mode-hook))
  (add-hook hook #'init-cider-set-lookup-command))

(defun init-cider-eval-sexp-to-comment ()
  "Eval sexp and insert result as comment."
  (interactive)
  (save-excursion
    (forward-sexp)
    (unless (eolp)
      (newline-and-indent))
    (cider-pprint-eval-last-sexp-to-comment)))

(defun init-cider-insert-sexp-to-repl (arg)
  "Insert sexp to repl.  ARG see `cider-insert-last-sexp-in-repl'."
  (interactive "P")
  (save-excursion
    (forward-sexp)
    (cider-insert-last-sexp-in-repl arg)))

(defun init-cider-format-sexp ()
  "Format sexp."
  (interactive)
  (save-excursion
    (forward-sexp)
    (cider-format-edn-last-sexp)))

(keymap-set cider-mode-map "C-c C-n" #'cider-repl-set-ns)
(keymap-set cider-mode-map "C-c C-i" #'init-cider-insert-sexp-to-repl)
(keymap-set cider-mode-map "C-c C-;" #'init-cider-eval-sexp-to-comment)
(keymap-set cider-mode-map "C-M-q" #'init-cider-format-sexp)
(keymap-set cider-repl-mode-map "C-M-q" #'init-cider-format-sexp)

;;; repl history

(add-to-list 'consult-mode-histories
             '(cider-repl-mode cider-repl-input-history cider-repl-input-history-position cider-repl-bol-mark))

(evil-define-key 'insert cider-repl-mode-map
  (kbd "M-r") #'consult-history)

;;; macrostep

(defun init-cider-macrostep-macro-form-p (_sexp _env)
  "Macro?"
  t)

(defun init-cider-macrostep-sexp-bounds ()
  "Find bounds of macro sexp."
  (interactive)
  (bounds-of-thing-at-point 'sexp))

(defun init-cider-macrostep-expand (sexp _env)
  "Expand SEXP using Cider."
  (or (cider-sync-request:macroexpand "macroexpand" sexp)
      (user-error "Macro expansion failed")))

(defun init-cider-macrostep-expand-1 (sexp _env)
  "Expand SEXP using Cider."
  (or (cider-sync-request:macroexpand "macroexpand-1" sexp)
      (user-error "Macro expansion failed")))

(defun init-cider-macrostep-insert (sexp _env)
  "Insert expanded SEXP."
  (insert (propertize sexp 'face 'macrostep-expansion-highlight-face)))

(defun init-cider-set-macrostep ()
  "Set Cider macroexpand backends."
  (setq-local macrostep-environment-at-point-function #'ignore)
  (setq-local macrostep-macro-form-p-function #'init-cider-macrostep-macro-form-p)
  (setq-local macrostep-sexp-bounds-function #'init-cider-macrostep-sexp-bounds)
  (setq-local macrostep-sexp-at-point-function #'buffer-substring-no-properties)
  (setq-local macrostep-expand-function #'init-cider-macrostep-expand)
  (setq-local macrostep-expand-1-function #'init-cider-macrostep-expand-1)
  (setq-local macrostep-print-function #'init-cider-macrostep-insert))

(add-hook 'cider-mode-hook #'init-cider-set-macrostep)
(add-hook 'cider-repl-mode-hook #'init-cider-set-macrostep)

(keymap-set cider-mode-map "C-c e" #'macrostep-expand)
(keymap-set cider-repl-mode-map "C-c e" #'macrostep-expand)

(provide 'init-clojure)
;;; init-clojure.el ends here
