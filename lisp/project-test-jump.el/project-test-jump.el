;;; project-test-jump.el --- Jump to test files in a project -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: convenience

;;; Commentary:

;; Jump between source and test files of the current buffer in a project.
;; Call `project-test-jump', which finds the function to jump by the file
;; extension in `project-test-jump-function-alist'.

;;; Code:

(require 'project)
(require 'subr-x)

(defvar project-test-jump-function-alist
  '(("clj" . project-test-jump-clojure-find-test-file)
    ("cljc" . project-test-jump-clojure-find-test-file)
    ("cljs" . project-test-jump-clojure-find-test-file))
  "Alist of (EXTENSION . FUNCTION) used by `project-test-jump'.
FUNCTION finds the test file of a buffer whose file name has
EXTENSION.  It is called with `default-directory' bound to the project
root.")

;;;###autoload
(defun project-test-jump ()
  "Find the test file of the current buffer in this project.
The function is looked up by the file extension in
`project-test-jump-function-alist'."
  (interactive)
  (unless buffer-file-name
    (user-error "No buffer file name found"))
  (let ((function (alist-get (file-name-extension buffer-file-name)
                             project-test-jump-function-alist
                             nil nil #'equal)))
    (unless function
      (user-error "No find test file function found"))
    (let ((default-directory (project-root (project-current t))))
      (funcall function))))

(defun project-test-jump-find-file (files)
  "Find the first existing file in FILES, or create the first one."
  (if-let* ((file (seq-find #'file-exists-p files)))
      (find-file file)
    (if-let* ((file (car files)))
        (find-file (read-file-name
                    "Create test file: "
                    (file-name-directory file)
                    nil nil
                    (file-name-nondirectory file)))
      (user-error "No test file found"))))

;;; clojure

(defvar project-test-jump-clojure-extensions '("cljc" "clj" "cljs")
  "Clojure file extensions.")

(defun project-test-jump-clojure-extensions (extension)
  "Return clojure file extensions, given EXTENSION first."
  (cons extension (remove extension project-test-jump-clojure-extensions)))

;; (project-test-jump-clojure-extensions "clj") => '("clj" "cljc" "cljs")
;; (project-test-jump-clojure-extensions "cljc") => '("cljc" "clj" "cljs")

(defun project-test-jump-clojure-file-with-extensions (file)
  "Return clojure files with different extension, given FILE first."
  (let ((base (file-name-sans-extension file))
        (extension (file-name-extension file)))
    (thread-last
      (project-test-jump-clojure-extensions extension)
      (seq-map (lambda (extension) (concat base "." extension))))))

;; (project-test-jump-clojure-file-with-extensions "foo/bar.clj")
;; => '("foo/bar.clj" "foo/bar.cljc" "foo/bar.cljs")
;; (project-test-jump-clojure-file-with-extensions "foo/bar.cljc")
;; => '("foo/bar.cljc" "foo/bar.clj" "foo/bar.cljs")

(defun project-test-jump-clojure-test-file (file)
  "Convert FILE to test file with same extension."
  (let ((file (concat "/" file)))
    (cond
     ((string-match "\\(.*?\\)/src/\\(.*\\)\\(\\.clj.?\\)$" file)
      (thread-first
        (concat (match-string 1 file) "/test/" (match-string 2 file) "_test" (match-string 3 file))
        (substring 1)))
     ((string-match "\\(.*?\\)/test/\\(.*\\)_test\\(\\.clj.?\\)$" file)
      (thread-first
        (concat (match-string 1 file) "/src/" (match-string 2 file) (match-string 3 file))
        (substring 1))))))

;; (project-test-jump-clojure-test-file "src/foo/bar.clj") => "test/foo/bar_test.clj"
;; (project-test-jump-clojure-test-file "test/foo/bar_test.clj") => "src/foo/bar.clj"
;; (project-test-jump-clojure-test-file "clojure/src/foo/bar.clj") => "clojure/test/foo/bar_test.clj"
;; (project-test-jump-clojure-test-file "clojure/test/foo/bar_test.clj") => "clojure/src/foo/bar.clj"

(defun project-test-jump-clojure-test-files (file)
  "Convert FILE to test files, with possible extensions."
  (when-let* ((file (project-test-jump-clojure-test-file file)))
    (project-test-jump-clojure-file-with-extensions file)))

;; (project-test-jump-clojure-test-files "src/foo/bar.clj")
;; => '("test/foo/bar_test.clj" "test/foo/bar_test.cljc" "test/foo/bar_test.cljs")
;; (project-test-jump-clojure-test-files "test/foo/bar_test.cljc")
;; => '("src/foo/bar.cljc" "src/foo/bar.clj" "src/foo/bar.cljs")

(defun project-test-jump-clojure-find-test-file ()
  "Find test file of current buffer."
  (if (not buffer-file-name)
      (user-error "No buffer file name found")
    (project-test-jump-find-file
     (project-test-jump-clojure-test-files (file-relative-name buffer-file-name)))))

(provide 'project-test-jump)
;;; project-test-jump.el ends here
