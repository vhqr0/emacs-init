;;; markdown-org-tests.el --- Tests for markdown-org.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.

;;; Code:

(require 'ert)
(require 'markdown-org)

;;; todo

(defun markdown-org-test-toggle (text)
  "Toggle TODO at the beginning of TEXT, return the result text."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (markdown-org-toggle-todo)
    (buffer-string)))

(ert-deftest markdown-org-test-toggle-todo ()
  (should (equal (markdown-org-test-toggle "## foo") "## TODO foo"))
  (should (equal (markdown-org-test-toggle "## TODO foo") "## foo"))
  (should (equal (markdown-org-test-toggle "#\tfoo") "#\tTODO foo"))
  (with-temp-buffer
    (insert "## foo")
    (goto-char (point-max))
    (markdown-org-toggle-todo)
    (should (equal (buffer-string) "## TODO foo"))
    (should (eolp)))
  (should-error (markdown-org-test-toggle "foo") :type 'user-error)
  (should-error (markdown-org-test-toggle "#foo") :type 'user-error))

;;; agenda

(ert-deftest markdown-org-test-agenda-faces ()
  (with-temp-buffer
    (markdown-org-agenda-mode)
    (let ((inhibit-read-only t))
      (insert "# TODO a\n### b\n######### c\n#d\n"))
    (font-lock-ensure)
    (should (equal (mapcar (lambda (pos) (get-text-property pos 'face))
                           '(1 2 3 7 8 10 12 13 14))
                   '(markdown-org-heading-marks nil markdown-org-todo nil nil
                     markdown-org-heading-marks markdown-org-heading-marks nil nil)))
    (goto-char (point-min))
    (forward-line 3)
    (should-not (text-property-not-all (point) (point-max) 'face nil))))

(provide 'markdown-org-tests)
;;; markdown-org-tests.el ends here
