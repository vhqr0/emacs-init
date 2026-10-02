;;; simple-abbrev.el --- Define abbrevs from a data file -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: abbrev, convenience

;;; Commentary:

;; Define abbrev tables from `simple-abbrev-file', a list of
;; (TABLENAME (ABBREV . EXPANSION) ...).  Call `simple-abbrev-load'.
;; EXPANSION may be:
;; - text: (text "expansion")
;; - yas: (yas "snippet" (ENVSYM ENVVAL) ...), which requires yasnippet.

;;; Code:

(declare-function yas-expand-snippet "yasnippet")

(defvar simple-abbrev-file
  (locate-user-emacs-file "abbrevs.eld")
  "File of abbrev definitions.")

(defun simple-abbrev-yas-define (table abbrev snippet &optional env)
  "Define an ABBREV in TABLE, to expand a yas SNIPPET with ENV."
  (let ((length (length abbrev))
        (hook (make-symbol abbrev))
        (ensure-pair (car (alist-get 'ensure-pair env))))
    (put hook 'no-self-insert t)
    (fset hook (lambda ()
                 (delete-char (- length))
                 (when (and ensure-pair (/= ?\( (char-before)))
                   (insert-pair 0 ?\( ?\)))
                 (yas-expand-snippet snippet nil nil env)))
    (define-abbrev table abbrev 'yas hook :system t)))

(defun simple-abbrev-define (table abbrev expansion)
  "Define an ABBREV in TABLE, to expand as EXPANSION.
EXPANSION may be:
- text: (text \"expansion\")
- yas: (yas \"expansion\" (ENVSYM ENVVAL) ...)"
  (let ((expansion-type (car expansion))
        (expansion (cdr expansion)))
    (cond ((eq expansion-type 'text)
           (define-abbrev table abbrev (car expansion) nil :system t))
          ((eq expansion-type 'yas)
           (simple-abbrev-yas-define table abbrev (car expansion) (cdr expansion)))
          (t
           (user-error "Invalid abbrev expansion type")))))

(defun simple-abbrev-define-table (tablename defs)
  "Define abbrev table with TABLENAME and abbrevs DEFS."
  (let ((table (if (boundp tablename) (symbol-value tablename))))
    (unless table
      (setq table (make-abbrev-table))
      (set tablename table))
    (unless (memq tablename abbrev-table-name-list)
      (push tablename abbrev-table-name-list))
    (dolist (def defs)
      (simple-abbrev-define table (car def) (cdr def)))))

;;;###autoload
(defun simple-abbrev-load (&optional file)
  "Load abbrevs FILE, defaulting to `simple-abbrev-file'."
  (interactive)
  (let ((file (or file simple-abbrev-file)))
    (when (file-exists-p file)
      (let ((defs (with-temp-buffer
                    (insert-file-contents file)
                    (read (buffer-string)))))
        (dolist (def defs)
          (simple-abbrev-define-table (car def) (cdr def)))))))

(provide 'simple-abbrev)
;;; simple-abbrev.el ends here
