;;; simple-format.el --- Format buffers with external programs -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: tools

;;; Commentary:

;; Format the buffer with an external program, reading the text from its
;; standard input and the formatted text from its standard output.  The
;; result is applied by `replace-region-contents', which diffs the texts
;; to keep point and markers.  Call `simple-format-buffer', which finds
;; the command by the major mode in `simple-format-command-function-alist'.
;; Without a command, the buffer is indented by `indent-region'.

;;; Code:

(defvar simple-format-command-function-alist
  '((c-mode . simple-format-clang-format-cpp-command)
    (c++-mode . simple-format-clang-format-cpp-command)
    (java-mode . simple-format-clang-format-java-command)
    (csharp-mode . simple-format-clang-format-csharp-command)
    (js-base-mode . simple-format-prettier-javascript-command)
    (typescript-ts-base-mode . simple-format-prettier-typescript-command)
    (css-base-mode . simple-format-prettier-css-command)
    (scss-mode . simple-format-prettier-scss-command)
    (html-mode . simple-format-prettier-html-command)
    (js-json-mode . simple-format-prettier-json-command)
    (json-mode . simple-format-prettier-json-command)
    (python-base-mode . simple-format-yapf-command)
    (clojure-mode . simple-format-cljfmt-command))
  "Alist of (MAJOR . FUNCTION) used by `simple-format-buffer'.
FUNCTION returns the formatter command, a list of the program and its
arguments, or nil if none, such as when the program is not found.  The
buffer text is sent to its standard input, and the formatted text is
read from its standard output.  A major mode uses the FUNCTION of its
nearest ancestor in this alist.")

(defvar simple-format-max-secs nil
  "MAX-SECS of `replace-region-contents', or nil for no limit.")

(defvar simple-format-max-costs nil
  "MAX-COSTS of `replace-region-contents', or nil for the default.")

(defconst simple-format-error-buffer-name "*simple-format errors*"
  "Name of the buffer showing formatter errors.")

(defun simple-format-show-error (command status error-file)
  "Show the error of COMMAND exiting with STATUS, with stderr in ERROR-FILE."
  (with-current-buffer (get-buffer-create simple-format-error-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "%s exited with %s\n\n" (string-join command " ") status))
      (insert-file-contents error-file)
      (special-mode))
    (display-buffer (current-buffer))))

(defun simple-format-run (command)
  "Format the accessible portion of the buffer with COMMAND."
  (let ((output (generate-new-buffer " *simple-format*" t))
        (error-file (make-temp-file "simple-format"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (unwind-protect
        (let ((status (apply #'call-process-region (point-min) (point-max) (car command)
                             nil (list output error-file) nil (cdr command))))
          (if (not (eql status 0))
              (progn
                (simple-format-show-error command status error-file)
                (user-error "Format failed, see %s" simple-format-error-buffer-name))
            (replace-region-contents (point-min) (point-max) output
                                     simple-format-max-secs simple-format-max-costs)
            (when-let* ((buffer (get-buffer simple-format-error-buffer-name)))
              (delete-windows-on buffer))))
      (kill-buffer output)
      (delete-file error-file))))

(defun simple-format-command ()
  "Return the formatter command of the major mode, or nil if none."
  (when-let* ((function (seq-some (lambda (major)
                                    (alist-get major simple-format-command-function-alist))
                                  (derived-mode-all-parents major-mode))))
    (funcall function)))

;;;###autoload
(defun simple-format-buffer ()
  "Format the buffer by `simple-format-command-function-alist'.
If there is no command, indent the buffer by `indent-region'."
  (interactive)
  (let ((command (simple-format-command)))
    (save-restriction
      (widen)
      (if command
          (simple-format-run command)
        (indent-region (point-min) (point-max))))))

;;; clang-format

(defvar simple-format-clang-format-program "clang-format"
  "Program of clang-format.")

(defun simple-format-clang-format-command (&optional extension)
  "Return the clang-format command for files of EXTENSION.
clang-format infers the language and finds its config by the assumed
file name, which is the buffer file name with EXTENSION, or stdin with
EXTENSION.  If EXTENSION is nil, it is the buffer file name, or stdin.c.
Return nil if clang-format is not found."
  (when (executable-find simple-format-clang-format-program)
    (list simple-format-clang-format-program
          "-assume-filename"
          (cond ((null extension) (or buffer-file-name "stdin.c"))
                (buffer-file-name (concat (file-name-sans-extension buffer-file-name) "." extension))
                (t (concat "stdin." extension))))))

(defun simple-format-clang-format-cpp-command ()
  "Return the clang-format command for C or C++."
  (simple-format-clang-format-command "cpp"))

(defun simple-format-clang-format-java-command ()
  "Return the clang-format command for Java."
  (simple-format-clang-format-command "java"))

(defun simple-format-clang-format-csharp-command ()
  "Return the clang-format command for C#."
  (simple-format-clang-format-command "cs"))

;;; prettier

(defvar simple-format-prettier-program "prettier"
  "Program of prettier.")

(defun simple-format-prettier-command (&optional parser)
  "Return the prettier command with PARSER.
If PARSER is nil, prettier infers it from the file name.  Return nil
if prettier is not found."
  (when (executable-find simple-format-prettier-program)
    (append (list simple-format-prettier-program)
            (when buffer-file-name
              (list "--stdin-filepath" buffer-file-name))
            (when parser
              (list (concat "--parser=" parser))))))

(defun simple-format-prettier-javascript-command ()
  "Return the prettier command for JavaScript."
  (simple-format-prettier-command "babel"))

(defun simple-format-prettier-typescript-command ()
  "Return the prettier command for TypeScript."
  (simple-format-prettier-command "typescript"))

(defun simple-format-prettier-css-command ()
  "Return the prettier command for CSS."
  (simple-format-prettier-command "css"))

(defun simple-format-prettier-scss-command ()
  "Return the prettier command for SCSS."
  (simple-format-prettier-command "scss"))

(defun simple-format-prettier-html-command ()
  "Return the prettier command for HTML."
  (simple-format-prettier-command "html"))

(defun simple-format-prettier-json-command ()
  "Return the prettier command for JSON."
  (simple-format-prettier-command "json"))

;;; yapf

(defvar simple-format-yapf-program "yapf"
  "Program of yapf.")

(defun simple-format-yapf-command ()
  "Return the yapf command, or nil if yapf is not found."
  (when (executable-find simple-format-yapf-program)
    (list simple-format-yapf-program)))

;;; cljfmt

(defvar simple-format-cljfmt-program "cljfmt"
  "Program of cljfmt.")

(defun simple-format-cljfmt-command ()
  "Return the cljfmt command, or nil if cljfmt is not found."
  (when (executable-find simple-format-cljfmt-program)
    (list simple-format-cljfmt-program "fix" "-")))

(provide 'simple-format)
;;; simple-format.el ends here
