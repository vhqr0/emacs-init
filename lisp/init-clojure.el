;;; init-clojure.el --- Init Clojure -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Clojure language.

;;; Code:

(require 'init-emacs)

;;; major mode

(require 'clojure-mode)

(defvar init-clojure-modes '(clojurec-mode clojure-mode clojurescript-mode))

(add-hook 'clojure-mode-hook #'init-elisp-set-outline)

(dolist (mode init-clojure-modes)
  (add-to-list 'init-evil-eval-function-alist `(,mode . cider-eval-region)))

;;; flymake

;; Ref: flymake-kondor https://github.com/turbo-cafe/flymake-kondor

(defvar init-clojure-kondo-program "clj-kondo")

(defconst init-clojure-kondo-diag-regexp "^.+:\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\([[:alpha:]]+\\): \\(.+\\)$")

(defvar-local init-clojure-kondo-proc nil)

(defun init-clojure-make-kondo-diag (buffer)
  "Build clj-kondo diag of BUFFER for Flymake."
  (let* ((row (string-to-number (match-string 1)))
         (col (string-to-number (match-string 2)))
         (type (let ((type (match-string 3)))
                 (cond ((string= type "error") :error) ((string= type "warning") :warning) (t :none))))
         (msg (match-string 4))
         (region (flymake-diag-region buffer row col)))
    (flymake-make-diagnostic buffer (car region) (cdr region) type msg)))

(defun init-clojure-make-kondo-diags (buffer)
  "Build clj-kondo diag list of BUFFER for flymake."
  (let (diags)
    (while (search-forward-regexp init-clojure-kondo-diag-regexp nil t)
      (push (init-clojure-make-kondo-diag buffer) diags))
    (nreverse diags)))

(defun init-clojure-make-kondo-proc (buffer report-fn)
  "Build the clj-kondo process of BUFFER for Flymake with REPORT-FN."
  (make-process
   :name "flymake-clj-kondo"
   :noquery t
   :connection-type 'pipe
   :buffer (generate-new-buffer "*flymake-clj-kondo*")
   :command (let ((lang (-if-let (file-name (buffer-file-name buffer)) (file-name-extension file-name) "clj")))
              `(,init-clojure-kondo-program "--lint" "-" "--lang" ,lang))
   :sentinel
   (lambda (proc _event)
     (when (memq (process-status proc) '(exit signal))
       (let ((proc-buffer (process-buffer proc)))
         (unwind-protect
             (if (with-current-buffer buffer
                   (eq proc init-clojure-kondo-proc))
                 (with-current-buffer proc-buffer
                   (goto-char (point-min))
                   (funcall report-fn (init-clojure-make-kondo-diags buffer)))
               (flymake-log :warning "Canceling obsolete checker %s" proc))
           (kill-buffer proc-buffer)))))))

(defun init-clojure-flymake-kondo-backend (report-fn &rest _args)
  "Build the Flymake backend for clj-kondo with REPORT-FN."
  (unless (executable-find init-clojure-kondo-program)
    (user-error "Executable kondo not found"))
  (when (process-live-p init-clojure-kondo-proc)
    (kill-process init-clojure-kondo-proc))
  (let ((buffer (current-buffer)))
    (save-restriction
      (widen)
      (let ((proc (init-clojure-make-kondo-proc buffer report-fn)))
        (setq init-clojure-kondo-proc proc)
        (process-send-region proc (point-min) (point-max))
        (process-send-eof proc)))))

(defun init-clojure-set-flymake ()
  "Set Flymake backend."
  (add-hook 'flymake-diagnostic-functions #'init-clojure-flymake-kondo-backend nil t))

(add-hook 'clojure-mode-hook #'init-clojure-set-flymake)
(add-hook 'clojure-mode-hook #'flymake-mode)

;;; cider

(setq! cider-mode-line '(:eval (format " Cider[%s]" (cider--modeline-info))))

(require 'cider)
(require 'cider-format)
(require 'cider-macroexpansion)

(dolist (map (list cider-mode-map cider-repl-mode-map))
  (define-key map [remap evil-lookup] #'cider-doc)
  (define-key map [remap evil-goto-definition] #'cider-find-var))

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

(dolist (map (list cider-mode-map cider-repl-mode-map))
  (keymap-set map "C-M-q" #'init-cider-format-sexp))

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

(dolist (hook '(cider-mode-hook cider-repl-mode-hook))
  (add-hook hook #'init-cider-set-macrostep))

(dolist (map (list cider-mode-map cider-repl-mode-map))
  (keymap-set map "C-c e" #'macrostep-expand))

(provide 'init-clojure)
;;; init-clojure.el ends here
