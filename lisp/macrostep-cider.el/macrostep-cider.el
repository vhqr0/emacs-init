;;; macrostep-cider.el --- Macrostep backend for Cider -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1") (macrostep "0.9") (cider "1.0"))
;; Version: 0.1.0
;; Keywords: languages, lisp

;;; Commentary:

;; Expand Clojure macros with `macrostep-expand' through Cider.
;; Add `macrostep-cider-setup' to `cider-mode-hook' and
;; `cider-repl-mode-hook'.

;;; Code:

(require 'macrostep)

(declare-function cider-sync-request:macroexpand "cider-macroexpansion")

(defun macrostep-cider-macro-form-p (_sexp _env)
  "Return non-nil, treating every sexp as a macro form."
  t)

(defun macrostep-cider-sexp-bounds ()
  "Find bounds of macro sexp."
  (interactive)
  (bounds-of-thing-at-point 'sexp))

(defun macrostep-cider-expand-1 (sexp _env)
  "Expand SEXP once using Cider."
  (or (cider-sync-request:macroexpand "macroexpand-1" sexp)
      (user-error "Macro expansion failed")))

(defun macrostep-cider-insert (sexp _env)
  "Insert expanded SEXP."
  (insert (propertize sexp 'face 'macrostep-expansion-highlight-face)))

;;;###autoload
(defun macrostep-cider-setup ()
  "Set Cider macroexpand backends."
  (setq-local macrostep-environment-at-point-function #'ignore)
  (setq-local macrostep-macro-form-p-function #'macrostep-cider-macro-form-p)
  (setq-local macrostep-sexp-bounds-function #'macrostep-cider-sexp-bounds)
  (setq-local macrostep-sexp-at-point-function #'buffer-substring-no-properties)
  (setq-local macrostep-expand-1-function #'macrostep-cider-expand-1)
  (setq-local macrostep-print-function #'macrostep-cider-insert))

(provide 'macrostep-cider)
;;; macrostep-cider.el ends here
