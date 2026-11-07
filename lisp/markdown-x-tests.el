;;; markdown-x-tests.el --- Tests for markdown-x.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.

;;; Code:

(require 'ert)
(require 'markdown-x)

;;; todo

(defun markdown-x-test-toggle (text)
  "Toggle TODO at the beginning of TEXT, return the result text."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (markdown-x-toggle-todo)
    (buffer-string)))

(ert-deftest markdown-x-test-toggle-todo ()
  (should (equal (markdown-x-test-toggle "## foo") "## TODO foo"))
  (should (equal (markdown-x-test-toggle "## TODO foo") "## foo"))
  (should (equal (markdown-x-test-toggle "#\tfoo") "#\tTODO foo"))
  (with-temp-buffer
    (insert "## foo")
    (goto-char (point-max))
    (markdown-x-toggle-todo)
    (should (equal (buffer-string) "## TODO foo"))
    (should (eolp)))
  (should-error (markdown-x-test-toggle "foo") :type 'user-error)
  (should-error (markdown-x-test-toggle "#foo") :type 'user-error))

(provide 'markdown-x-tests)
;;; markdown-x-tests.el ends here
