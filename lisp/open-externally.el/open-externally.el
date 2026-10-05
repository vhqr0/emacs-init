;;; open-externally.el --- Open files with external applications -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: files, convenience

;;; Commentary:

;; Open files, directories and URLs with external applications.
;; - `open-externally' opens the buffer file, or `default-directory'.
;; - `open-externally-at-point' opens the file or URL at point, or the
;;   marked files in Dired.
;; The opening is done by `open-externally-function'.

;;; Code:

(require 'ffap)

(declare-function dired-get-marked-files "dired")
(declare-function shell-command-do-open "dired-aux")

(defvar open-externally-function #'shell-command-do-open
  "Function opening a list of files or URLs with external applications.")

(defun open-externally-files (files)
  "Open FILES with external applications.
Expand file names, but keep URLs as they are."
  (let ((files (mapcar (lambda (file)
                         (if (string-match-p "\\`[a-z]+://" file)
                             file
                           (expand-file-name file)))
                       files)))
    (message "Opening %s externally..." (string-join files ", "))
    (funcall open-externally-function files)))

;;;###autoload
(defun open-externally ()
  "Open the buffer file, or `default-directory', externally."
  (interactive)
  (open-externally-files (list (or buffer-file-name default-directory))))

;;;###autoload
(defun open-externally-at-point ()
  "Open the file or URL at point externally.
In Dired, open the marked files, or the file at point."
  (interactive)
  (open-externally-files
   (if (derived-mode-p 'dired-mode)
       (dired-get-marked-files)
     (list (or (ffap-guesser)
               (user-error "No file or URL at point"))))))

(provide 'open-externally)
;;; open-externally.el ends here
