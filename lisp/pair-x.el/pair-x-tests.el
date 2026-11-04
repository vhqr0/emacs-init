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
  (should (equal (pair-x-test "(a (b |c) d)" #'pair-x-splice-sexp) "(a b |c d)"))
  (should (equal (pair-x-test "(a [b |c])" #'pair-x-splice-sexp) "(a b |c)"))
  ;; In a string, the list enclosing it.
  (should (equal (pair-x-test "(a \"b |c\")" #'pair-x-splice-sexp) "a \"b |c\""))
  (should (equal (pair-x-test "(foo\n (bar|\n  baz))" #'pair-x-splice-sexp)
                 "(foo\n bar|\n baz)"))
  (should-error (pair-x-test "a |b" #'pair-x-splice-sexp) :type 'user-error))

(ert-deftest pair-x-test-forward-slurp ()
  (should (equal (pair-x-test "(a (b|) c d)" #'pair-x-forward-slurp-sexp) "(a (b| c) d)"))
  (should (equal (pair-x-test "((b|) \"c d\")" #'pair-x-forward-slurp-sexp) "((b| \"c d\"))"))
  ;; Over the comment before the sexp.
  (should (equal (pair-x-test "(a (b|) ; x\n c)" #'pair-x-forward-slurp-sexp)
                 "(a (b| ; x\n    c))"))
  (should-error (pair-x-test "(a (b|))" #'pair-x-forward-slurp-sexp) :type 'user-error)
  (should-error (pair-x-test "(b|)" #'pair-x-forward-slurp-sexp) :type 'user-error))

(ert-deftest pair-x-test-forward-barf ()
  (should (equal (pair-x-test "(a (b| c d))" #'pair-x-forward-barf-sexp) "(a (b| c) d)"))
  (should (equal (pair-x-test "((|b))" #'pair-x-forward-barf-sexp) "((|)b)"))
  ;; Not into the comment after the previous sexp.
  (should (equal (pair-x-test "(a (b| ; x\n c))" #'pair-x-forward-barf-sexp)
                 "(a (b|) ; x\n   c)"))
  (should-error (pair-x-test "(a (|))" #'pair-x-forward-barf-sexp) :type 'user-error))

(ert-deftest pair-x-test-backward-slurp ()
  (should (equal (pair-x-test "(a b (c|) d)" #'pair-x-backward-slurp-sexp) "(a (b c|) d)"))
  (should (equal (pair-x-test "(a (|c))" #'pair-x-backward-slurp-sexp) "((a |c))"))
  (should-error (pair-x-test "(|c)" #'pair-x-backward-slurp-sexp) :type 'user-error))

(ert-deftest pair-x-test-backward-barf ()
  (should (equal (pair-x-test "(a (b c|) d)" #'pair-x-backward-barf-sexp) "(a b (c|) d)"))
  (should (equal (pair-x-test "((b|))" #'pair-x-backward-barf-sexp) "(b|())"))
  (should-error (pair-x-test "(a (|))" #'pair-x-backward-barf-sexp) :type 'user-error))

(provide 'pair-x-tests)
;;; pair-x-tests.el ends here
