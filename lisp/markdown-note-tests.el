;;; markdown-note-tests.el --- Tests for markdown-note.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.

;;; Code:

(require 'ert)
(require 'markdown-note)

(defconst markdown-note-test-id "20230730T182013")

(defun markdown-note-test-file-name (title keywords)
  "Return the file name of the test note of TITLE and KEYWORDS."
  (markdown-note--file-name markdown-note-test-id title
                            (markdown-note--keywords keywords)))

(ert-deftest markdown-note-test-file-name ()
  (should (equal (markdown-note-test-file-name "Hello, World!" '("sysutil" "Remote"))
                 "20230730T182013--hello-world__remote_sysutil.md"))
  (should (equal (markdown-note-test-file-name "tmux" nil)
                 "20230730T182013--tmux.md"))
  (should (equal (markdown-note-test-file-name "tmux" '("c++" "x-y"))
                 "20230730T182013--tmux__c++_x-y.md"))
  (should-error (markdown-note-test-file-name "tmux" '("a_b")) :type 'user-error)
  (should-error (markdown-note-test-file-name "tmux" '("a.b")) :type 'user-error))

(ert-deftest markdown-note-test-file-id ()
  (should (equal (markdown-note--file-id
                  (markdown-note-test-file-name "tmux" '("remote" "sysutil")))
                 markdown-note-test-id))
  (should (equal (markdown-note--file-id
                  (concat "/notes/" (markdown-note-test-file-name "tmux" nil)))
                 markdown-note-test-id))
  (should-not (markdown-note--file-id "20230730T182013--tmux.org"))
  (should-not (markdown-note--file-id "20230730T182013--tmux__.md"))
  (should-not (markdown-note--file-id "tmux.md")))

(defun markdown-note-test-front-matter (title keywords)
  "Insert the front matter of the test note of TITLE and KEYWORDS."
  (insert (markdown-note--front-matter
           markdown-note-test-id "2023-07-30T18:20:13+08:00" title keywords)))

(defun markdown-note-test-title-and-tags ()
  "Return the title and tags in the front matter of the buffer."
  (list (markdown-note--parse-title (markdown-note--value "title"))
        (markdown-note--parse-tags (markdown-note--value "tags"))))

(ert-deftest markdown-note-test-front-matter ()
  (with-temp-buffer
    (markdown-note-test-front-matter "a \"b\" c" '("remote" "sysutil"))
    (insert "title: body\n")
    (should (equal (markdown-note-test-title-and-tags)
                   '("a \"b\" c" ("remote" "sysutil"))))
    (markdown-note--set-value "title" (format "%S" "new"))
    (markdown-note--set-value "tags" (markdown-note--format-tags nil))
    (should (equal (markdown-note-test-title-and-tags) '("new" nil)))
    (should (equal (buffer-substring (pos-bol 0) (point-max)) "title: body\n"))))

(ert-deftest markdown-note-test-front-matter-invalid ()
  (dolist (text '("title: \"tmux\"\ntags: []\n"
                  "---\ntitle: \"tmux\"\n---\n"
                  "---\ntitle: tmux\ntags: []\n---\n"
                  "---\ntitle: \"tmux\"\ntags: [remote]\n---\n"))
    (with-temp-buffer
      (insert text)
      (should-error (markdown-note-test-title-and-tags) :type 'user-error))))

(ert-deftest markdown-note-test-link ()
  (should (equal (markdown-note--parse-link "denote:20230730T182013")
                 markdown-note-test-id))
  (should-not (markdown-note--parse-link "denote:20230730T182013x"))
  (should-not (markdown-note--parse-link "https://example.com")))

(provide 'markdown-note-tests)
;;; markdown-note-tests.el ends here
