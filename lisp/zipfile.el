;;; zipfile.el --- Visit the files in zip archives by vim names -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: files

;;; Commentary:

;; `zipfile-file-name-handler' lets Emacs visit the files in zip
;; archives, such as jar, by the names as of the zip plugin of vim:
;;
;;   zipfile:/a/b.jar::c/d.clj
;;   zipfile:///a/b.jar::c/d.clj
;;
;; The latter is a URI, percent-decoded, as returned by clojure-lsp with
;; `:dependency-scheme' "zipfile" by default, such as by finding a
;; definition in a library.  Eglot passes the URIs not of "file" to
;; Emacs as they are, so with `eglot-extend-to-xref', the server
;; manages the files visited.
;;
;; The files are extracted by `archive-zip-extract' of arc-mode, and
;; visited read only.  Their directory is that of the archive.  Loading
;; this library adds the handler to `file-name-handler-alist'.

;;; Code:

(require 'arc-mode)
(require 'url-util)

(defconst zipfile-regexp "\\`zipfile:"
  "Regexp of the names handled by `zipfile-file-name-handler'.")

(defun zipfile--parse (name)
  "Return (ARCHIVE . FILE) of NAME, or signal an error if invalid."
  (let ((path (if (string-match "\\`zipfile://" name)
                  ;; A URI, maybe with the colons also encoded.
                  (decode-coding-string (url-unhex-string (substring name (match-end 0)))
                                        'utf-8-unix)
                (string-remove-prefix "zipfile:" name))))
    ;; Without the slash before the drive letter, as `eglot-uri-to-path'.
    (when (and (eq system-type 'windows-nt)
               (string-match-p "\\`/[A-Za-z]:" path))
      (setq path (substring path 1)))
    (if (string-match "\\`\\(.+?\\)::\\(.+\\)\\'" path)
        (cons (match-string 1 path) (match-string 2 path))
      (error "Invalid zipfile name: %s" name))))

(defun zipfile--insert (name visit replace)
  "Insert the file of NAME, as `insert-file-contents' with VISIT and REPLACE."
  (pcase-let ((`(,archive . ,file) (zipfile--parse name))
              (contents nil))
    (unless (file-readable-p archive)
      (signal 'file-missing (list "Opening zip archive" "No such file" archive)))
    (setq contents (with-temp-buffer
                     (archive-zip-extract archive file)
                     (buffer-string)))
    (when replace
      (erase-buffer))
    (save-excursion
      (insert contents))
    (when visit
      (setq buffer-file-name name)
      (set-buffer-modified-p nil))
    (list name (length contents))))

;;;###autoload
(defun zipfile-file-name-handler (operation &rest args)
  "Handle OPERATION with ARGS on the zipfile names.
See `file-name-handler-alist'."
  (let ((name (car args)))
    (pcase operation
      ((or 'expand-file-name 'file-truename 'abbreviate-file-name
           'substitute-in-file-name)
       name)
      ('file-name-directory
       (file-name-directory (car (zipfile--parse name))))
      ('file-name-nondirectory
       (file-name-nondirectory (cdr (zipfile--parse name))))
      ((or 'file-exists-p 'file-readable-p)
       (file-readable-p (car (zipfile--parse name))))
      ('verify-visited-file-modtime t)
      ((or 'file-writable-p 'file-directory-p 'file-accessible-directory-p
           'file-executable-p 'file-symlink-p 'file-remote-p 'file-attributes
           'file-modes 'file-name-case-insensitive-p 'file-newer-than-file-p
           'make-auto-save-file-name 'set-visited-file-modtime 'vc-registered
           'lock-file 'unlock-file 'file-locked-p)
       nil)
      ('insert-file-contents
       (zipfile--insert name (nth 1 args) (nth 4 args)))
      (_
       (let ((inhibit-file-name-handlers
              (cons #'zipfile-file-name-handler
                    (and (eq inhibit-file-name-operation operation)
                         inhibit-file-name-handlers)))
             (inhibit-file-name-operation operation))
         (apply operation args))))))

(add-to-list 'file-name-handler-alist
             (cons zipfile-regexp #'zipfile-file-name-handler))

(provide 'zipfile)
;;; zipfile.el ends here
