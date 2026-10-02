;;; flymake-x.el --- Generic Flymake backend for external checkers -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: tools

;;; Commentary:

;; A generic Flymake backend running an external checker on the buffer.
;; Set `flymake-x-make-command-function' and
;; `flymake-x-make-report-function' locally, then add `flymake-x-backend'
;; to `flymake-diagnostic-functions'.

;;; Code:

(require 'flymake)

(defvar-local flymake-x-make-command-function nil
  "Function returning the checker command, or nil to skip checking.")

(defvar-local flymake-x-make-report-function nil
  "Function making diagnostics from the checker output.
It is called with the source buffer in the checker output buffer.")

(defvar-local flymake-x-proc nil
  "Current checker process.")

(defun flymake-x-make-proc (buffer report-fn)
  "Make Flymake process for BUFFER.
REPORT-FN see `flymake-x-backend'."
  (when-let* ((make-command-function (buffer-local-value 'flymake-x-make-command-function buffer)))
    (when-let* ((command (funcall make-command-function)))
      (let* ((proc-buffer-name (format "*flymake-x for %s*" (buffer-name buffer)))
             (sentinel
              (lambda (proc _event)
                (when (memq (process-status proc) '(exit signal))
                  (let ((proc-buffer (process-buffer proc)))
                    (unwind-protect
                        (if (eq proc (buffer-local-value 'flymake-x-proc buffer))
                            (let ((make-report-function (buffer-local-value 'flymake-x-make-report-function buffer)))
                              (with-current-buffer buffer
                                (save-excursion
                                  (save-restriction
                                    (widen)
                                    (with-current-buffer proc-buffer
                                      (widen)
                                      (goto-char (point-min))
                                      (funcall report-fn (funcall make-report-function buffer)))))))
                          (flymake-log :warning "Canceling obsolete checker %s" proc))
                      (kill-buffer proc-buffer)))))))
        (make-process
         :name proc-buffer-name
         :noquery t
         :connection-type 'pipe
         :buffer (generate-new-buffer-name proc-buffer-name)
         :command command
         :sentinel sentinel)))))

;;;###autoload
(defun flymake-x-backend (report-fn &rest _args)
  "Generic Flymake backend.
REPORT-FN see `flymake-diagnostic-functions'."
  (when-let* ((proc (flymake-x-make-proc (current-buffer) report-fn)))
    (when (process-live-p flymake-x-proc)
      (kill-process flymake-x-proc))
    (setq flymake-x-proc proc)
    (save-restriction
      (widen)
      (process-send-region proc (point-min) (point-max))
      (process-send-eof proc))))

;;; clj-kondo

(defvar flymake-x-clj-kondo-program "clj-kondo"
  "Program of clj-kondo.")

(defun flymake-x-clj-kondo-make-command ()
  "Make clj-kondo command."
  (when (executable-find flymake-x-clj-kondo-program)
    (let* ((buffer-file-name (buffer-file-name))
           (lang (if (not buffer-file-name)
                     "clj"
                   (file-name-extension buffer-file-name))))
      `(,flymake-x-clj-kondo-program
        "--lint" "-"
        "--lang" ,lang
        ,@(when buffer-file-name
            `("--filename" ,buffer-file-name))))))

(defconst flymake-x-clj-kondo-diag-regexp
  "^.+:\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\([[:alpha:]]+\\): \\(.+\\)$"
  "Regexp matching a clj-kondo diagnostic.")

(defvar flymake-x-clj-kondo-type-alist
  '(("error" . :error) ("warning" . :warning))
  "Alist of clj-kondo levels to Flymake types.")

(defun flymake-x-clj-kondo-make-report (buffer)
  "Make Flymake report for clj-kondo in source BUFFER."
  (let (diags)
    (while (search-forward-regexp flymake-x-clj-kondo-diag-regexp nil t)
      (let* ((row (string-to-number (match-string 1)))
             (col (string-to-number (match-string 2)))
             (type (or (cdr (assoc (match-string 3) flymake-x-clj-kondo-type-alist)) :note))
             (msg (match-string 4))
             (region (flymake-diag-region buffer row col))
             (diag (flymake-make-diagnostic buffer (car region) (cdr region) type msg)))
        (push diag diags)))
    (nreverse diags)))

;;;###autoload
(defun flymake-x-clj-kondo-setup ()
  "Set clj-kondo Flymake backend."
  (setq-local flymake-x-make-command-function #'flymake-x-clj-kondo-make-command)
  (setq-local flymake-x-make-report-function #'flymake-x-clj-kondo-make-report)
  (add-hook 'flymake-diagnostic-functions #'flymake-x-backend nil t))

(provide 'flymake-x)
;;; flymake-x.el ends here
