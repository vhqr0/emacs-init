;;; markdown-x-tests.el --- Tests for markdown-x.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.

;;; Code:

(require 'ert)
(require 'markdown-x)

(defmacro markdown-x-test-with-dir (&rest body)
  "Run BODY with `default-directory' bound to a temporary directory."
  (declare (indent 0))
  `(let ((default-directory (file-name-as-directory (make-temp-file "markdown-x-test" t))))
     (unwind-protect
         (progn ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (file-in-directory-p file default-directory)
             (with-current-buffer buffer
               (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory default-directory t))))

(defun markdown-x-test-write (file content)
  "Write CONTENT to FILE."
  (with-temp-file file
    (insert content)))

(defun markdown-x-test-read (file)
  "Return the content of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

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

;;; agenda

(ert-deftest markdown-x-test-agenda-headings ()
  (markdown-x-test-with-dir
    (make-directory "notes")
    (markdown-x-test-write "notes/a.md" "# a\ntext\n```sh\n# comment\n```\n## TODO b\n")
    (markdown-x-test-write "notes/b.txt" "# ignored\n")
    (markdown-x-test-write "c.md" "~~~\n# comment\n~~~\n# c\n")
    (let ((markdown-x-agenda-files '("notes" "c.md")))
      (should (equal (markdown-x-agenda-headings)
                     `((,(expand-file-name "notes/a.md") 1 "# a")
                       (,(expand-file-name "notes/a.md") 6 "## TODO b")
                       (,(expand-file-name "c.md") 4 "# c")))))))

(ert-deftest markdown-x-test-agenda-headings-default ()
  (markdown-x-test-with-dir
    (markdown-x-test-write "notes.md" "# TODO a\n")
    (let ((markdown-x-agenda-files nil)
          (markdown-x-capture-default-file (expand-file-name "notes.md")))
      (should (equal (markdown-x-agenda-headings)
                     `((,(expand-file-name "notes.md") 1 "# TODO a")))))))

(ert-deftest markdown-x-test-agenda-headings-buffer ()
  (markdown-x-test-with-dir
    (markdown-x-test-write "a.md" "# a\n")
    (with-current-buffer (find-file-noselect "a.md")
      (goto-char (point-max))
      (insert "# unsaved\n"))
    (let ((markdown-x-agenda-files '("a.md")))
      (should (equal (mapcar #'caddr (markdown-x-agenda-headings))
                     '("# a" "# unsaved"))))))

(ert-deftest markdown-x-test-agenda-todo ()
  (markdown-x-test-with-dir
    (markdown-x-test-write "a.md" "# a\n## TODO b\n## TODO c\n")
    (let ((markdown-x-agenda-files '("a.md"))
          (file (expand-file-name "a.md")))
      (save-window-excursion
        (markdown-x-agenda-todo)
        (with-current-buffer "*markdown-x-agenda-todo*"
          (should (eq major-mode 'markdown-x-agenda-mode))
          (should (equal (buffer-string) "## TODO b\n## TODO c\n"))
          (goto-char (point-min))
          (forward-line 1)
          (should (equal (markdown-x-agenda-heading) (list file 3 "## TODO c")))
          (markdown-x-agenda-toggle-todo)
          (should (equal (buffer-string) "## TODO b\n## c\n"))
          (should (equal (markdown-x-agenda-heading) (list file 3 "## c")))
          (should (equal (line-number-at-pos) 2))
          (with-current-buffer (get-file-buffer file)
            (should (equal (buffer-string) "# a\n## TODO b\n## c\n")))
          (markdown-x-agenda-toggle-todo)
          (should (equal (buffer-string) "## TODO b\n## TODO c\n"))
          (with-current-buffer (get-file-buffer file)
            (should (equal (buffer-string) "# a\n## TODO b\n## TODO c\n")))
          (revert-buffer)
          (should (equal (buffer-string) "## TODO b\n## TODO c\n"))
          (goto-char (point-min))
          (markdown-x-agenda-goto)
          (should (equal (buffer-file-name) file))
          (should (equal (line-number-at-pos) 2)))))))

(ert-deftest markdown-x-test-agenda-todo-changed ()
  (markdown-x-test-with-dir
    (markdown-x-test-write "a.md" "## TODO a\n## TODO b\n")
    (let ((markdown-x-agenda-files '("a.md")))
      (save-window-excursion
        (markdown-x-agenda-todo)
        (with-current-buffer (find-file-noselect "a.md")
          (goto-char (point-min))
          (insert "intro\n"))
        (with-current-buffer "*markdown-x-agenda-todo*"
          (goto-char (point-min))
          (should-error (markdown-x-agenda-toggle-todo) :type 'user-error)
          (should (equal (buffer-string) "## TODO a\n## TODO b\n"))
          (with-current-buffer (get-file-buffer (expand-file-name "a.md"))
            (should (equal (buffer-string) "intro\n## TODO a\n## TODO b\n")))
          (revert-buffer)
          (goto-char (point-min))
          (markdown-x-agenda-toggle-todo)
          (with-current-buffer (get-file-buffer (expand-file-name "a.md"))
            (should (equal (buffer-string) "intro\n## a\n## TODO b\n"))))))))

;;; capture

(defun markdown-x-test-capture (keys text finish)
  "Capture by KEYS, insert TEXT, then finish if FINISH, or abort."
  (save-window-excursion
    (let ((unread-command-events (listify-key-sequence keys))
          (read-char-choice-use-read-key t))
      (markdown-x-capture))
    (should markdown-x-capture-mode)
    (insert text)
    (if finish
        (markdown-x-capture-finalize)
      (markdown-x-capture-abort))))

(ert-deftest markdown-x-test-capture ()
  (markdown-x-test-with-dir
    (markdown-x-test-write "inbox.md" "# inbox")
    (let ((markdown-x-capture-templates
           '((?t "Todo" "inbox.md" "## TODO $0\n")
             (?n "Note" "notes.md" "## ${1:title}\n$0"))))
      (markdown-x-test-capture "t" "foo" t)
      (should (equal (markdown-x-test-read "inbox.md") "# inbox\n## TODO foo\n"))
      (markdown-x-test-capture "t" "bar" nil)
      (should (equal (markdown-x-test-read "inbox.md") "# inbox\n## TODO foo\n"))
      (should-not (get-buffer "*markdown-x-capture*"))
      (markdown-x-test-capture "n" "baz" t)
      (should (equal (markdown-x-test-read "notes.md") "## baz\n")))
    (let ((markdown-x-capture-templates nil)
          (markdown-x-capture-default-file (expand-file-name "default.md")))
      (with-current-buffer (find-file-noselect "inbox.md")
        (goto-char (point-min))
        (forward-line 1)
        (markdown-x-test-capture "" "qux" t))
      (should (string-match-p
               (concat "\\`# TODO qux\n"
                       "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Z][a-z]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}\n"
                       (regexp-quote (expand-file-name "inbox.md")) " 2\n"
                       "```\n## TODO foo\n```\n\\'")
               (markdown-x-test-read "default.md"))))))

(ert-deftest markdown-x-test-capture-origin ()
  (markdown-x-test-with-dir
    (markdown-x-test-write "src.md" "one\ntwo three\n")
    (let ((markdown-x-capture-templates
           '((?o "Origin" "inbox.md"
                 "`(buffer-name markdown-x-capture-origin-buffer)` `(file-name-nondirectory markdown-x-capture-origin-file)`:`markdown-x-capture-origin-line`: `markdown-x-capture-origin-line-text` [`markdown-x-capture-origin-region`]\n")
             (?b "Buffer" "inbox.md"
                 "`(prin1-to-string markdown-x-capture-origin-file)`:`markdown-x-capture-origin-line`: `markdown-x-capture-origin-line-text` [`(prin1-to-string markdown-x-capture-origin-region)`]\n"))))
      (with-current-buffer (find-file-noselect "src.md")
        (transient-mark-mode 1)
        (goto-char (point-min))
        (forward-line 1)
        (set-mark (point))
        (forward-word 1)
        (activate-mark)
        (markdown-x-test-capture "o" "" t))
      (should (equal (markdown-x-test-read "inbox.md") "src.md src.md:2: two three [two]\n"))
      (with-temp-buffer
        (insert "scratch")
        (markdown-x-test-capture "b" "" t))
      (should (string-suffix-p "\nnil:1: scratch [nil]\n" (markdown-x-test-read "inbox.md"))))))

(provide 'markdown-x-tests)
;;; markdown-x-tests.el ends here
