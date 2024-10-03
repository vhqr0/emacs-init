;;; init-clojure --- Init Clojure -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Clojure language.

;;; Code:

(require 'init-emacs)

;;; major mode

(require 'clojure-mode)

(defvar init-clojure-modes '(clojurec-mode clojure-mode clojurescript-mode))
(defvar init-clojure-mode-hooks '(clojurec-mode-hook clojure-mode-hook clojurescript-mode-hook))

;;; flycheck

(require 'flycheck-clj-kondo)

(dolist (mode init-clojure-modes)
  (add-to-list 'init-evil-eval-function-alist `(,mode . cider-eval-region)))

;;; cider

(setq! cider-mode-line '(:eval (format " Cider[%s]" (cider--modeline-info))))

(require 'cider)

(defun init-lookup-setup-cider ()
  "Setup cider doc."
  (init-lookup-setup-command #'cider-doc))

(dolist (hook '(cider-mode-hook cider-repl-mode-hook))
  (add-hook hook #'init-lookup-setup-cider))

;;; repl history

(defun init-counsel-cider-repl-history ()
  "Browse Cider REPL history."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (setq ivy-completion-beg beg)
    (setq ivy-completion-end end)
    (ivy-read "History: " (ivy-history-contents cider-repl-input-history)
              :keymap ivy-reverse-i-search-map
              :initial-input (buffer-substring beg end)
              :action #'counsel--browse-history-action
              :caller #'init-counsel-cider-repl-history)))

(evil-define-key 'insert cider-repl-mode-map
  (kbd "M-r") #'init-counsel-cider-repl-history)

;;; macrostep

(require 'cider-macroexpansion)

(defun init-cider-macrostep-sexp-bounds ()
  "Find bounds of macro sexp."
  (interactive)
  (let ((thing (sp-get-thing)))
    (cons (sp-get thing :beg) (sp-get thing :end))))

(defun init-cider-macrostep-expand (sexp _env)
  "Expand SEXP using cider."
  (or (cider-sync-request:macroexpand "macroexpand" sexp)
      (user-error "Macro expansion failed")))

(defun init-cider-macrostep-expand-1 (sexp _env)
  "Expand SEXP using cider."
  (or (cider-sync-request:macroexpand "macroexpand-1" sexp)
      (user-error "Macro expansion failed")))

(defun init-cider-macrostep-insert (sexp _env)
  "Insert expanded SEXP."
  (insert (propertize sexp 'face 'macrostep-expansion-highlight-face)))

(defun init-cider-set-macrostep ()
  "Set cider macroexpand backends."
  (setq-local macrostep-environment-at-point-function #'ignore)
  (setq-local macrostep-sexp-bounds-function #'init-cider-macrostep-sexp-bounds)
  (setq-local macrostep-sexp-at-point-function #'buffer-substring-no-properties)
  (setq-local macrostep-expand-function #'init-cider-macrostep-expand)
  (setq-local macrostep-expand-1-function #'init-cider-macrostep-expand-1)
  (setq-local macrostep-print-function #'init-cider-macrostep-insert))

(dolist (hook init-clojure-mode-hooks)
  (add-hook hook #'init-cider-set-macrostep))

;;; context leader

(defun init-cider-eval-sexp-to-comment ()
  "Eval sexp and insert result as comment."
  (interactive)
  (save-excursion
    (sp-forward-sexp)
    (unless (eolp)
      (newline-and-indent))
    (cider-pprint-eval-last-sexp-to-comment)))

;; co-work with lsp-mode, inhibit these keys: wghra=FTG
(defvar-keymap init-cider-command-map
  "e" #'macrostep-expand
  "n" #'cider-repl-set-ns
  ";" #'init-cider-eval-sexp-to-comment)

(init-leader-minor-mode-set 'cider-mode
  "y" init-cider-command-map)

(provide 'init-clojure)
;;; init-clojure.el ends here
