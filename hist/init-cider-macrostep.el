;;; Package --- Cider support for Macrostep

;;; Commentary:

;; This package add Cider support for Macrostep.  To use this package,
;; append these lines to init.el:
;;
;; (require 'init-cider-macrostep)
;; (add-hook 'cider-mode-hook #'init-cider-set-macrostep)
;; (add-hook 'cider-repl-mode-hook #'init-cider-set-macrostep)
;; (keymap-set cider-mode-map "C-c e" #'macrostep-expand)
;; (keymap-set cider-repl-mode-map "C-c e" #'macrostep-expand)

;;; Code:

(require 'macrostep)
(require 'cider)

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

(provide 'init-macrostep-cider)
;;; init-cider-macrostep.el ends here
