;;; myswiper-tests.el --- Tests for myswiper -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.

;;; Code:

(require 'ert)
(require 'myswiper)

(defmacro myswiper-test-with-buffer (text &rest body)
  "Run BODY in a buffer of TEXT shown in the selected window.
The histories and the global mark ring are not changed."
  (declare (indent 1))
  `(let ((buffer (generate-new-buffer "*myswiper-test*"))
         (myswiper-history nil)
         (extended-command-history nil)
         (global-mark-ring nil))
     (unwind-protect
         (save-window-excursion
           (switch-to-buffer buffer)
           (insert ,text)
           (goto-char (point-min))
           ,@body)
       (kill-buffer buffer))))

(defun myswiper-test-run (keys)
  "Run `myswiper' typing KEYS."
  (execute-kbd-macro (vconcat (kbd "M-x myswiper RET") (kbd keys))))

(ert-deftest myswiper-test-collect ()
  (with-temp-buffer
    (insert "foo\nbar\nbaz bar\n")
    (should (equal (myswiper--collect "ba") [5 9]))
    (should (equal (myswiper--collect "") [1 5 9]))
    (should (equal (myswiper--collect "^") [1 5 9]))
    (should (equal (myswiper--collect "qux") []))
    ;; Ignore case unless the input has an upper case letter.
    (should (equal (myswiper--collect "FOO") []))
    (should (equal (myswiper--collect "Foo") []))
    (should (equal (myswiper--collect "foo") [1]))
    (let ((case-fold-search t))
      (should (equal (myswiper--collect "foo") [1]))
      (goto-char (point-min))
      (insert "FOO\n")
      (should (equal (myswiper--collect "foo") [1 5]))
      (should (equal (myswiper--collect "FOO") [1])))))

(ert-deftest myswiper-test-nearest ()
  (should (null (myswiper--nearest [] 1)))
  (should (= (myswiper--nearest [1 5 9] 1) 0))
  (should (= (myswiper--nearest [1 5 9] 4) 1))
  (should (= (myswiper--nearest [1 5 9] 9) 2))
  (should (= (myswiper--nearest [1 5 9] 10) 2)))

(ert-deftest myswiper-test-jump ()
  (myswiper-test-with-buffer "foo\nbar\nbaz\n"
    (myswiper-test-run "ba C-n RET")
    (should (= (point) 9))
    (should (equal (car myswiper-history) "ba"))
    ;; The mark at the start.
    (should (= (mark t) 1))
    (should-not (seq-some (lambda (overlay) (overlay-get overlay 'myswiper))
                          (overlays-in (point-min) (point-max))))))

(ert-deftest myswiper-test-render ()
  (myswiper-test-with-buffer "foo\nbar\nbaz\n"
    (let (count shown)
      ;; Get what is shown, by a key bound in the minibuffer.
      (let ((myswiper-map
             (define-keymap :parent myswiper-map
               "C-c C-t" (lambda ()
                           (interactive)
                           (setq count (overlay-get myswiper--count-overlay 'before-string)
                                 shown (overlay-get myswiper--overlay 'after-string))))))
        (myswiper-test-run "ba C-n C-c C-t C-g"))
      (should (equal count "2/2 "))
      (should (equal (substring-no-properties shown) " \n2 bar\n3 baz"))
      ;; The selected line, and the match.
      (should (memq 'myswiper-current-line
                    (ensure-list (get-text-property (string-search "3 baz" shown) 'face shown))))
      (should (memq 'myswiper-match
                    (ensure-list (get-text-property (string-search "bar" shown) 'face shown)))))))

(ert-deftest myswiper-test-at-point ()
  (myswiper-test-with-buffer "a*b\naab\na*b\n"
    (emacs-lisp-mode)
    ;; Quoted, not matching "aab".
    (execute-kbd-macro (kbd "M-x myswiper-at-point RET C-n RET"))
    (should (= (point) 9))
    (should (equal (car myswiper-history) "a\\*b"))))

(ert-deftest myswiper-test-move-hook ()
  (myswiper-test-with-buffer "foo\nbar\nbaz\n"
    (let (moved)
      (let ((myswiper-move-hook (list (lambda () (push (point) moved)))))
        (myswiper-test-run "ba C-n C-g"))
      ;; At the start, to the nearest line, then to the next line.
      (should (equal (seq-uniq (reverse moved)) '(1 5 9))))))

(ert-deftest myswiper-test-nearest-line ()
  (myswiper-test-with-buffer "bar\nfoo\nbar\n"
    (forward-line 1)
    ;; The first line not before the current line.
    (myswiper-test-run "bar RET")
    (should (= (point) 9))))

(ert-deftest myswiper-test-quit ()
  (myswiper-test-with-buffer "foo\nbar\nbaz\n"
    (forward-line 1)
    ;; Quitting in a keyboard macro does not signal `quit'.
    (myswiper-test-run "baz C-g")
    (should (= (point) 5))
    (should-not (seq-some (lambda (overlay) (overlay-get overlay 'myswiper))
                          (overlays-in (point-min) (point-max))))))

(ert-deftest myswiper-test-no-line ()
  ;; Not by a keyboard macro, which would be terminated by the error,
  ;; leaving the minibuffer waiting for input.
  (with-temp-buffer
    (should-error (myswiper-exit) :type 'user-error)))

(ert-deftest myswiper-test-occur ()
  (myswiper-test-with-buffer "foo\nbar\nbaz\n"
    (myswiper-test-run "ba C-x C-s")
    (should (= (point) 1))
    (with-current-buffer "*Occur*"
      (should (string-match-p "^2 matches" (buffer-string))))
    (kill-buffer "*Occur*")))

(provide 'myswiper-tests)
;;; myswiper-tests.el ends here
