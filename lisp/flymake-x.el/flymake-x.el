;;; flymake-x.el --- Generic Flymake backend for external checkers -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: tools

;;; Commentary:

;; A generic Flymake backend running an external checker on the buffer.
;; Add `flymake-x-backend' to `flymake-diagnostic-functions' locally, such
;; as by `flymake-x-setup' in a mode hook.  It finds the checker by the
;; major mode in `flymake-x-function-alist'.

;;; Code:

(require 'flymake)

(defvar flymake-x-function-alist
  '((clojure-mode flymake-x-clj-kondo-make-command flymake-x-clj-kondo-make-report))
  "Alist of (MAJOR COMMAND-FUNCTION REPORT-FUNCTION) used by `flymake-x-backend'.
COMMAND-FUNCTION returns the checker command, or nil to skip checking.
The buffer contents are sent to its standard input.  REPORT-FUNCTION is
called with the source buffer in the checker output buffer, and
returns the diagnostics.  A major mode uses the functions of its
nearest ancestor in this alist.")

(defvar-local flymake-x-proc nil
  "Current checker process.")

(defun flymake-x-functions ()
  "Return the (COMMAND-FUNCTION REPORT-FUNCTION) of the major mode."
  (seq-some (lambda (major)
              (alist-get major flymake-x-function-alist))
            (derived-mode-all-parents major-mode)))

(defun flymake-x-make-proc (buffer command report-function report-fn)
  "Make Flymake process running COMMAND for BUFFER.
REPORT-FUNCTION makes the diagnostics, see `flymake-x-function-alist'.
REPORT-FN see `flymake-x-backend'."
  (let* ((proc-buffer-name (format "*flymake-x for %s*" (buffer-name buffer)))
         (sentinel
          (lambda (proc _event)
            (when (memq (process-status proc) '(exit signal))
              (let ((proc-buffer (process-buffer proc)))
                (unwind-protect
                    (if (eq proc (buffer-local-value 'flymake-x-proc buffer))
                        (with-current-buffer buffer
                          (save-excursion
                            (save-restriction
                              (widen)
                              (with-current-buffer proc-buffer
                                (widen)
                                (goto-char (point-min))
                                (funcall report-fn (funcall report-function buffer))))))
                      (flymake-log :warning "Canceling obsolete checker %s" proc))
                  (kill-buffer proc-buffer)))))))
    (make-process
     :name proc-buffer-name
     :noquery t
     :connection-type 'pipe
     :buffer (generate-new-buffer-name proc-buffer-name)
     :command command
     :sentinel sentinel)))

;;;###autoload
(defun flymake-x-backend (report-fn &rest _args)
  "Generic Flymake backend.
The checker is found in `flymake-x-function-alist'.  REPORT-FN see
`flymake-diagnostic-functions'."
  (pcase-let* ((`(,command-function ,report-function) (flymake-x-functions))
               (command (and command-function (funcall command-function))))
    (if (null command)
        (funcall report-fn nil)
      (let ((proc (flymake-x-make-proc (current-buffer) command report-function report-fn)))
        (when (process-live-p flymake-x-proc)
          (kill-process flymake-x-proc))
        (setq flymake-x-proc proc)
        (save-restriction
          (widen)
          (process-send-region proc (point-min) (point-max))
          (process-send-eof proc))))))

;;;###autoload
(defun flymake-x-setup ()
  "Add `flymake-x-backend' to `flymake-diagnostic-functions' locally."
  (add-hook 'flymake-diagnostic-functions #'flymake-x-backend nil t))

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

(provide 'flymake-x)
;;; flymake-x.el ends here
