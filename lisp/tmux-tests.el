;;; tmux-tests.el --- Tests for tmux -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.

;;; Code:

(require 'ert)
(require 'tmux)

(ert-deftest tmux-test-parse-panes ()
  (should (equal (tmux--parse-panes
                  (concat "%1\t1:0.0\tclaude\t80x24\t✳ a\tb\n"
                          "%1\t1-2:0.0\tclaude\t80x24\t✳ a\tb\n"
                          "%2\t1:1.0\tbash\t80x24\t\n"))
                 '(("%1" ["1:0.0" "claude" "80x24" "✳ a\tb"])
                   ("%2" ["1:1.0" "bash" "80x24" ""])))))

(ert-deftest tmux-test-paste-args ()
  (should (equal (tmux--paste-args "%1" nil)
                 '("load-buffer" "-b" "tmux-el" "-"
                   ";" "paste-buffer" "-p" "-d" "-b" "tmux-el" "-t" "%1")))
  (should (equal (last (tmux--paste-args "%1" t) 5)
                 '(";" "send-keys" "-t" "%1" "Enter"))))

(ert-deftest tmux-test-capture-args ()
  (should (equal (last (tmux--capture-args "%1" 10) 2) '("-S" "-10")))
  (should (equal (last (tmux--capture-args "%1" t) 2) '("-S" "-"))))

(ert-deftest tmux-test-log ()
  (let ((tmux-log-buffer-name " *tmux-test-log*"))
    (unwind-protect
        (let ((first (tmux--log "%1" "a\nb"))
              (second (tmux--log "%2" "c")))
          (tmux--log-status first "pushed")
          (tmux--log-status second "failed: x")
          (should (string-match-p
                   (concat "\\`\\* .* %1 pushed\na\nb\n\n"
                           "\\* .* %2 failed: x\nc\n\n\\'")
                   (with-current-buffer tmux-log-buffer-name (buffer-string)))))
      (kill-buffer tmux-log-buffer-name))))

(provide 'tmux-tests)
;;; tmux-tests.el ends here
