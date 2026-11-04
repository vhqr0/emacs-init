;;; pair-x-tests.el --- Tests for pair-x -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.

;;; Code:

(require 'ert)
(require 'pair-x)

(defun pair-x-test (text command)
  "Return TEXT after COMMAND in `emacs-lisp-mode'.
In TEXT and the result, \"|\" is point."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert text)
    (goto-char (point-min))
    (search-forward "|")
    (delete-char -1)
    (funcall command)
    (insert "|")
    (buffer-string)))

(ert-deftest pair-x-test-splice ()
  (should (equal (pair-x-test "(a (b |c) d)" #'pair-x-splice) "(a b |c d)"))
  (should (equal (pair-x-test "(a [b |c])" #'pair-x-splice) "(a b |c)"))
  ;; In a string, the list enclosing it.
  (should (equal (pair-x-test "(a \"b |c\")" #'pair-x-splice) "a \"b |c\""))
  (should (equal (pair-x-test "(foo\n (bar|\n  baz))" #'pair-x-splice)
                 "(foo\n bar|\n baz)"))
  (should-error (pair-x-test "a |b" #'pair-x-splice) :type 'user-error))

(ert-deftest pair-x-test-forward-slurp ()
  (should (equal (pair-x-test "(a (b|) c d)" #'pair-x-forward-slurp) "(a (b| c) d)"))
  (should (equal (pair-x-test "((b|) \"c d\")" #'pair-x-forward-slurp) "((b| \"c d\"))"))
  ;; Over the comment before the sexp.
  (should (equal (pair-x-test "(a (b|) ; x\n c)" #'pair-x-forward-slurp)
                 "(a (b| ; x\n    c))"))
  (should-error (pair-x-test "(a (b|))" #'pair-x-forward-slurp) :type 'user-error)
  (should-error (pair-x-test "(b|)" #'pair-x-forward-slurp) :type 'user-error))

(ert-deftest pair-x-test-forward-barf ()
  (should (equal (pair-x-test "(a (b| c d))" #'pair-x-forward-barf) "(a (b| c) d)"))
  (should (equal (pair-x-test "((|b))" #'pair-x-forward-barf) "((|)b)"))
  ;; Not into the comment after the previous sexp.
  (should (equal (pair-x-test "(a (b| ; x\n c))" #'pair-x-forward-barf)
                 "(a (b|) ; x\n   c)"))
  (should-error (pair-x-test "(a (|))" #'pair-x-forward-barf) :type 'user-error))

(ert-deftest pair-x-test-backward-slurp ()
  (should (equal (pair-x-test "(a b (c|) d)" #'pair-x-backward-slurp) "(a (b c|) d)"))
  (should (equal (pair-x-test "(a (|c))" #'pair-x-backward-slurp) "((a |c))"))
  (should-error (pair-x-test "(|c)" #'pair-x-backward-slurp) :type 'user-error))

(ert-deftest pair-x-test-backward-barf ()
  (should (equal (pair-x-test "(a (b c|) d)" #'pair-x-backward-barf) "(a b (c|) d)"))
  (should (equal (pair-x-test "((b|))" #'pair-x-backward-barf) "(b|())"))
  (should-error (pair-x-test "(a (|))" #'pair-x-backward-barf) :type 'user-error))

(provide 'pair-x-tests)
;;; pair-x-tests.el ends here
