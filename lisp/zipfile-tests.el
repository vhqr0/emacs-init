;;; zipfile-tests.el --- Tests for zipfile -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.

;;; Code:

(require 'ert)
(require 'zipfile)

(defconst zipfile-test-archive
  (expand-file-name "zipfile.el" (file-name-directory (locate-library "zipfile")))
  "A readable file, as an archive extracted by a stub.")

(defmacro zipfile-test-with-extract (&rest body)
  "Run BODY with `archive-zip-extract' inserting the test contents."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'archive-zip-extract)
              (lambda (archive file)
                (should (equal archive zipfile-test-archive))
                (should (equal file "a/b.clj"))
                (insert "(ns a.b)\n"))))
     ,@body))

(ert-deftest zipfile-test-parse ()
  (should (equal (zipfile--parse "zipfile:/a/b.jar::c/d.clj") '("/a/b.jar" . "c/d.clj")))
  (should (equal (zipfile--parse "zipfile:///a/b.jar::c/d.clj") '("/a/b.jar" . "c/d.clj")))
  ;; Decoded only in a URI, also the colons.
  (should (equal (zipfile--parse "zipfile:///a%20b/c.jar%3A%3Ad/%E4%B8%AD.clj")
                 '("/a b/c.jar" . "d/\u4e2d.clj")))
  (should (equal (zipfile--parse "zipfile:/a%20b.jar::c") '("/a%20b.jar" . "c")))
  (let ((system-type 'windows-nt))
    (should (equal (zipfile--parse "zipfile:///c:/a/b.jar::c/d.clj") '("c:/a/b.jar" . "c/d.clj")))
    (should (equal (zipfile--parse "zipfile:/c:/a/b.jar::c/d.clj") '("c:/a/b.jar" . "c/d.clj")))
    (should (equal (zipfile--parse "zipfile:c:/a/b.jar::c/d.clj") '("c:/a/b.jar" . "c/d.clj"))))
  (should-error (zipfile--parse "zipfile:/a/b.jar")))

(ert-deftest zipfile-test-handler ()
  (dolist (name (list (concat "zipfile:" zipfile-test-archive "::a/b.clj")
                      (concat "zipfile://" zipfile-test-archive "::a/b.clj")))
    (should (equal (zipfile-file-name-handler 'expand-file-name name) name))
    (should (equal (zipfile-file-name-handler 'substitute-in-file-name name) name))
    (should (equal (zipfile-file-name-handler 'file-name-nondirectory name) "b.clj"))
    (should (equal (zipfile-file-name-handler 'file-name-directory name)
                   (file-name-directory zipfile-test-archive)))
    (should (zipfile-file-name-handler 'file-readable-p name))
    (should-not (zipfile-file-name-handler 'file-writable-p name))
    (zipfile-test-with-extract
      (with-temp-buffer
        (should (equal (zipfile-file-name-handler 'insert-file-contents name t nil nil nil)
                       (list name 9)))
        (should (equal (buffer-string) "(ns a.b)\n"))
        (should (equal buffer-file-name name))
        (should-not (buffer-modified-p)))))
  (let ((name "zipfile:/nonexistent/b.jar::a/b.clj"))
    (should-not (zipfile-file-name-handler 'file-exists-p name))
    (with-temp-buffer
      (should-error (zipfile-file-name-handler 'insert-file-contents name nil nil nil nil)
                    :type 'file-missing))))

(provide 'zipfile-tests)
;;; zipfile-tests.el ends here
