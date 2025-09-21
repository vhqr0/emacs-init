;;; Package --- A tiny framework for Flymake

;;; Commentary:

;; This package impl a tiny framework for Flymake.  We also add
;; clj-kondo support for Clojure, append these lines to init.el:

;; (add-hook 'clojure-mode-hook #'init-clojure-set-kondo)
;; (add-hook 'clojure-mode-hook #'flymake-mode)

;;; Code:

(require 'flymake)

(defvar-local init-flymake-find-executable-function nil)
(defvar-local init-flymake-make-command-function nil)
(defvar-local init-flymake-report-diags-function nil)

(defvar-local init-flymake-proc nil)

(defun init-flymake-make-proc (buffer report-fn)
  "Make Flymake process for BUFFER.
REPORT-FN see `init-flymake-backend'."
  (when-let* ((find-executable-fn (buffer-local-value 'init-flymake-find-executable-function buffer)))
    (when (funcall find-executable-fn buffer)
      (let* ((proc-buffer-name (format "*init-flymake for %s*" (buffer-name buffer)))
             (make-command-fn (buffer-local-value 'init-flymake-make-command-function buffer))
             (report-diags-fn (buffer-local-value 'init-flymake-report-diags-function buffer))
             (command (funcall make-command-fn buffer))
             (sentinel
              (lambda (proc _event)
                (when (memq (process-status proc) '(exit signal))
                  (let ((proc-buffer (process-buffer proc)))
                    (unwind-protect
                        (if (eq proc (buffer-local-value 'init-flymake-proc buffer))
                            (funcall report-diags-fn buffer proc-buffer report-fn)
                          (flymake-log :warning "Canceling obsolete checker %s" proc))
                      (kill-buffer proc-buffer)))))))
        (make-process
         :name proc-buffer-name
         :noquery t
         :connection-type 'pipe
         :buffer (generate-new-buffer-name proc-buffer-name)
         :command command
         :sentinel sentinel)))))

(defun init-flymake-backend (report-fn &rest _args)
  "Generic Flymake backend.
REPORT-FN see `flymake-diagnostic-functions'."
  (when-let* ((proc (init-flymake-make-proc (current-buffer) report-fn)))
    (when (process-live-p init-flymake-proc)
      (kill-process init-flymake-proc))
    (setq init-flymake-proc proc)
    (save-restriction
      (widen)
      (process-send-region proc (point-min) (point-max))
      (process-send-eof proc))))

;;; kondo

(defvar init-clojure-kondo-program "clj-kondo")

(defun init-clojure-kondo-find-executable (_buffer)
  "Find kondo executable."
  (executable-find init-clojure-kondo-program))

(defun init-clojure-kondo-make-command (buffer)
  "Make kondo command for BUFFER."
  (let ((buffer-file-name (buffer-file-name buffer))
        (lang (if (not buffer-file-name)
                  "clj"
                (file-name-extension buffer-file-name))))
    `(,init-clojure-kondo-program
      "--lint" "-"
      "--lang" ,lang
      ,@(when buffer-file-name
          `("--filename" ,buffer-file-name)))))

(defconst init-clojure-kondo-diag-regexp
  "^.+:\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\([[:alpha:]]+\\): \\(.+\\)$")

(defvar init-clojure-kondo-type-alist
  '(("error" . :error) ("warning" . :warning)))

(defun init-clojure-kondo-make-diag (buffer)
  "Make Flymake diag for BUFFER from `match-string'."
  (let* ((row (string-to-number (match-string 1)))
         (col (string-to-number (match-string 2)))
         (type (cdr (assoc (match-string 3) init-clojure-kondo-type-alist)))
         (msg (match-string 4))
         (region (flymake-diag-region buffer row col)))
    (flymake-make-diagnostic
     buffer (car region) (cdr region) (or type :none) msg)))

(defun init-clojure-kondo-make-diags (buffer)
  "Make Flymake diags for BUFFER."
  (let (diags)
    (while (search-forward-regexp init-clojure-kondo-diag-regexp nil t)
      (push (init-clojure-kondo-make-diag buffer) diags))
    (nreverse diags)))

(defun init-clojure-kondo-report-diags (buffer proc-buffer report-fn)
  "Report Flymake diags in PROC-BUFFER for BUFFER.
REPORT-FN see `init-flymake-make-proc'."
  (funcall report-fn
           (with-current-buffer proc-buffer
             (widen)
             (goto-char (point-min))
             (init-clojure-kondo-make-diags buffer))))

(defun init-clojure-set-kondo ()
  "Set kondo Flymake backend."
  (setq-local init-flymake-find-executable-function #'init-clojure-kondo-find-executable)
  (setq-local init-flymake-make-command-function #'init-clojure-kondo-make-command)
  (setq-local init-flymake-report-diags-function #'init-clojure-kondo-report-diags)
  (add-hook 'flymake-diagnostic-functions #'init-flymake-backend nil t))

(provide 'init-flymake)
;;; init-flymake.el ends here
