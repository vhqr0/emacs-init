;;; iselect-tests.el --- Tests for iselect -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.

;;; Code:

(require 'ert)
(require 'iselect)

(defconst iselect-test-candidates
  '(("foo bar" . 1) ("Foo baz" . 2) ("qux" . 3)))

(defvar iselect-test--args nil
  "Arguments of `iselect-read' in `iselect-test--read'.")

(defvar iselect-test--result nil
  "Result of `iselect-test--read'.")

(defvar iselect-test-buffer nil
  "Buffer of the candidates of the last `iselect-test'.")

(defun iselect-test--read ()
  "Call `iselect-read' on the test candidates, for `iselect-test'."
  (interactive)
  (setq iselect-test--result
        (condition-case nil
            (apply #'iselect-read "> " iselect-test-candidates
                   `(,@iselect-test--args
                     :layout nil :buffer ,iselect-test-buffer :delay -1))
          (quit 'quit)
          (user-error 'user-error))))

(defmacro iselect-deftest (name args &rest body)
  "Define test NAME with ARGS running BODY without changing the global state.
`<f12>' calls `iselect-test--read' in BODY, and the buffer of the
candidates is killed after BODY."
  (declare (indent 2))
  `(ert-deftest ,name ,args
     (let ((emulation-mode-map-alists
            (cons `((t . ,(define-keymap "<f12>" #'iselect-test--read)))
                  emulation-mode-map-alists))
           (minibuffer-history nil)
           (iselect-test--args nil)
           (iselect-test--result nil)
           (iselect-test-buffer nil))
       (unwind-protect
           (save-window-excursion
             ,@body)
         (when (buffer-live-p iselect-test-buffer)
           (kill-buffer iselect-test-buffer))))))

(defun iselect-test (keys &rest args)
  "Call `iselect-read' on the test candidates with KEYS as input.
ARGS are passed to `iselect-read'.  The candidates are updated only by
`iselect-update', unless ARGS has `:delay', and then at once, as the
timers do not run in keyboard macros.  Return `quit' on quit, and
`user-error' on user errors.  The buffer of the candidates is kept in
`iselect-test-buffer' until the next call."
  (when (buffer-live-p iselect-test-buffer)
    (kill-buffer iselect-test-buffer))
  (setq iselect-test--args args
        iselect-test--result nil
        iselect-test-buffer (generate-new-buffer " *iselect-test*"))
  ;; The minibuffer reads from stdin in batch unless in a keyboard macro.
  (cl-letf (((symbol-function 'run-with-timer)
             (lambda (_secs _repeat function &rest args)
               (apply function args)
               nil)))
    (execute-kbd-macro (key-parse (concat "<f12> " keys))))
  iselect-test--result)

(defun iselect-test-filter (input &optional limit)
  "Return the data of the test candidates matching INPUT, at most LIMIT."
  (mapcar #'cddr (iselect--filter input iselect-test-candidates limit)))

(ert-deftest iselect-test-filter ()
  (should (equal (iselect-test-filter "") '(1 2 3)))
  (should (equal (iselect-test-filter "foo") '(1 2)))
  ;; Not ignoring case with an upper case letter.
  (should (equal (iselect-test-filter "Foo") '(2)))
  (should (equal (iselect-test-filter "o\\Wb") '(1 2)))
  (let ((case-fold-search nil))
    (should (equal (iselect-test-filter "foo") '(1)))
    (should (equal (iselect-test-filter "Foo") '(2))))
  (should (equal (iselect-test-filter "fo ba") '(1 2)))
  (should (equal (iselect-test-filter "fo  z") '(2)))
  (should (equal (iselect-test-filter "ba fo") nil))
  (should (equal (iselect-test-filter "o\\s-b") '(1 2)))
  (should (equal (iselect-test-filter "^q") '(3)))
  (should (equal (iselect-test-filter "r$\\|x") '(1 3)))
  (should (equal (iselect-test-filter "" 2) '(1 2)))
  (should (equal (iselect-test-filter "o" 0) nil))
  (should (equal (iselect-test-filter "o" -1) nil)))

(iselect-deftest iselect-test-select ()
  (should (equal (iselect-test "RET") '("foo bar" . 1)))
  (should (equal (iselect-test "q C-c C-u RET") '("qux" . 3)))
  (should (equal (iselect-test "q RET") '("foo bar" . 1)))
  (should (equal (iselect-test "C-n RET") '("Foo baz" . 2)))
  (should (equal (iselect-test "C-n C-p C-p RET") '("foo bar" . 1)))
  (should (equal (iselect-test "C-n C-n C-n C-n RET") '("qux" . 3)))
  (should (equal (iselect-test "f o o C-c C-u C-n RET") '("Foo baz" . 2)))
  (should (equal (iselect-test "C-n C-n f C-c C-u RET") '("foo bar" . 1)))
  (should (equal (iselect-test "<down> <down> <up> C-j") '("Foo baz" . 2)))
  (should (equal (iselect-test "M-> RET") '("qux" . 3)))
  (should (equal (iselect-test "M-> M-< RET") '("foo bar" . 1)))
  (should (equal (iselect-test "f C-c C-u M-> RET") '("Foo baz" . 2)))
  (should (equal (iselect-test "x x C-c C-u DEL DEL q C-c C-u RET") '("qux" . 3)))
  (should (equal (iselect-test "RET" :initial-input "baz") '("Foo baz" . 2))))

(iselect-deftest iselect-test-line-start ()
  (dolist (keys '("C-n C-n C-n C-g" "M-> C-g" "M-> C-p C-n C-g"))
    (iselect-test keys)
    (with-current-buffer iselect-test-buffer
      (should (= (point) (save-excursion
                           (goto-char (point-max))
                           (line-beginning-position)))))))

(iselect-deftest iselect-test-invalid-regexp ()
  (should (equal (iselect-test "[ C-M-j" :input-fn (lambda (s) (cons s 0)))
                 '("[" . 0)))
  (should (equal (iselect-test "q [ C-c C-u DEL C-c C-u RET") '("qux" . 3)))
  (should (eq (iselect-test "[ C-c C-u C-g") 'quit))
  (with-current-buffer iselect-test-buffer
    (should (equal (buffer-string) ""))))

(iselect-deftest iselect-test-limit ()
  (should (equal (iselect-test "C-n RET" :limit 2) '("Foo baz" . 2)))
  (should (equal (iselect-test "q C-c C-u RET" :limit 1) '("qux" . 3))))

(iselect-deftest iselect-test-no-match ()
  (with-temp-buffer
    (setq iselect--buffer (current-buffer))
    (switch-to-buffer (current-buffer))
    (should-error (iselect-exit) :type 'user-error)))

(iselect-deftest iselect-test-input ()
  (should (equal (iselect-test "x y C-M-j" :input-fn (lambda (s) (cons s 0)))
                 '("xy" . 0)))
  (should (eq (iselect-test "q C-M-j") 'user-error)))

(iselect-deftest iselect-test-quit ()
  (should (eq (iselect-test "f C-g") 'quit))
  (should (null (iselect-test "f C-M-c"))))

(iselect-deftest iselect-test-newline ()
  (let ((iselect-test-candidates '(("a\nb" . 1) ("b" . 2))))
    (should (equal (iselect-test "C-n RET") '("b" . 2)))
    (should (equal (iselect-test "a C-c C-u RET") '("a\nb" . 1)))
    (should (equal (iselect-test "a SPC b C-c C-u C-g") 'quit))
    (with-current-buffer iselect-test-buffer
      (should (equal (buffer-string) "")))
    (iselect-test "RET")
    (with-current-buffer iselect-test-buffer
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "a b\nb"))
      (should (equal (get-text-property 2 'display) "^J")))))

(iselect-deftest iselect-test-auto-update ()
  (should (equal (iselect-test "q RET" :delay 0) '("qux" . 3)))
  (should (equal (iselect-test "f o o C-n RET" :delay 0.2) '("Foo baz" . 2))))

(iselect-deftest iselect-test-suspend ()
  (should (equal (iselect-test "C-c C-s q C-c C-s RET") '("qux" . 3)))
  (should (equal (iselect-test "q C-c C-s DEL b a C-c C-s RET" :delay 0) '("foo bar" . 1)))
  (should (equal (iselect-test "C-c C-s q RET" :delay 0) '("foo bar" . 1)))
  (should (equal (iselect-test "C-c C-s q C-c C-u RET") '("qux" . 3)))
  (iselect-test "C-c C-s q C-g" :delay 0)
  (with-current-buffer iselect-test-buffer
    (should (equal (count-lines (point-min) (point-max)) 3))))

(iselect-deftest iselect-test-render ()
  (let ((iselect-test-candidates
         (mapcar (lambda (i) (cons (format "c%d" i) i)) (number-sequence 1 30))))
    (iselect-test "1 C-c C-u C-n C-n RET")
    (with-current-buffer iselect-test-buffer
      (should (equal (split-string (buffer-string) "\n")
                     '("c1" "c10" "c11" "c12" "c13" "c14" "c15"
                       "c16" "c17" "c18" "c19" "c21"))))
    (iselect-test "RET" :limit 20)
    (with-current-buffer iselect-test-buffer
      (should (= (count-lines (point-min) (point-max)) 20)))
    (iselect-test "x C-c C-u C-g")
    (with-current-buffer iselect-test-buffer
      (should (equal (buffer-string) "")))))

(provide 'iselect-tests)
;;; iselect-tests.el ends here
