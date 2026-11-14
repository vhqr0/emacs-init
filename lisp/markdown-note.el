;;; markdown-note.el --- Notes in markdown files -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: outlines, hypermedia

;;; Commentary:

;; Notes in markdown files of `markdown-note-directory', named as
;; ID--TITLE__KEYWORD1_KEYWORD2.md, where ID is the time of creation,
;; such as 20230730T182013.  TITLE is slugged as by denote.  The
;; keywords are in lower case and sorted, and may have only letters,
;; digits, + and -.
;;
;;   (setq markdown-note-directory "~/notes/")
;;
;; `markdown-note-new' reads the title and the keywords separated by
;; spaces, and shows a buffer of the new note with the front matter of
;; denote.  `C-c C-c' writes the buffer to the file of the note, which
;; must not exist yet, and visits it, and `C-c C-k' aborts.  The buffer is in
;; `text-mode', to change it:
;;
;;   (setq markdown-note-markdown-major-mode #'markdown-mode)
;;
;; `markdown-note-rename' reads the title and the keywords, initially
;; the title and tags in the front matter, then updates them in the
;; front matter, renames the file of the note, keeping the ID, and saves
;; it.  The front matter is parsed by lines as written by
;; `markdown-note-new': the title is a double quoted string, and the
;; tags are a list of them in one line, such as ["a", "b"].
;;
;; A link to a note is [TITLE](denote:ID), as by denote.
;; `markdown-note-link' reads a note and inserts a link to it, with the
;; title in its front matter.  `markdown-note-resolve-link' returns the
;; file of the note of a link, such as to follow it in `markdown-mode':
;;
;;   (defun my-markdown-note-follow-link (url)
;;     (when-let* ((file (markdown-note-resolve-link url)))
;;       (find-file file)))
;;
;;   (add-hook 'markdown-follow-link-functions #'my-markdown-note-follow-link)
;;
;; `markdown-note-backlinks' shows the links to the current note in the
;; notes, by `markdown-note-backlinks-function' called with the link,
;; by default `lgrep' in `markdown-note-directory'.

;;; Code:

(defgroup markdown-note nil
  "Notes in markdown files."
  :group 'text)

(defcustom markdown-note-directory (expand-file-name "notes" user-emacs-directory)
  "Directory of the notes."
  :type 'directory)

(defcustom markdown-note-markdown-major-mode #'text-mode
  "Major mode for markdown, such as `markdown-mode'."
  :type 'function)

(defun markdown-note--slug-title (title)
  "Return the slug of TITLE, as by denote."
  (downcase
   (replace-regexp-in-string
    "^-\\|-$" ""
    (replace-regexp-in-string
     "-\\{2,\\}" "-"
     (replace-regexp-in-string
      "_\\|\s+" "-"
      (replace-regexp-in-string "[][{}!@#$%^&*()+'\"?,.\|;:~`‘’“”/=]*" "" title))))))

(defun markdown-note--slug-keyword (keyword)
  "Return the slug of KEYWORD, in lower case.
Signal an error unless KEYWORD has only letters, digits, + and -."
  (unless (string-match-p "\\`[[:alnum:]+-]+\\'" keyword)
    (user-error "Invalid keyword: %s" keyword))
  (downcase keyword))

(defun markdown-note--keywords (keywords)
  "Return the slugs of KEYWORDS, sorted."
  (sort (mapcar #'markdown-note--slug-keyword keywords) #'string-collate-lessp))

(defun markdown-note--file-name (id title keywords)
  "Return the file name of the note of ID, TITLE and slugged KEYWORDS."
  (concat id "--" (markdown-note--slug-title title)
          (and keywords (concat "__" (string-join keywords "_")))
          ".md"))

(defconst markdown-note--file-name-regexp
  "\\`\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)--[^_]*\\(?:__[^_]+\\(?:_[^_]+\\)*\\)?\\.md\\'"
  "Regexp matching the file name of a note, with the ID as group 1.")

(defun markdown-note--file-id (file)
  "Return the ID of the note FILE, or nil if not named as a note."
  (let ((name (file-name-nondirectory file)))
    (and (string-match markdown-note--file-name-regexp name)
         (match-string 1 name))))

(defun markdown-note--format-tags (keywords)
  "Return the value of tags of KEYWORDS in the front matter."
  (format "[%s]" (mapconcat (lambda (keyword) (format "%S" keyword)) keywords ", ")))

(defun markdown-note--parse-title (value)
  "Return the title of VALUE in the front matter, a quoted string."
  (pcase (ignore-errors (read-from-string value))
    ((and `(,title . ,end)
          (guard (stringp title))
          (guard (string-blank-p (substring value end))))
     title)
    (_ (user-error "Invalid title in the front matter: %s" value))))

(defun markdown-note--parse-tags (value)
  "Return the keywords of VALUE of tags in the front matter, such as [\"a\", \"b\"]."
  (unless (string-match "\\`\\[\\(.*\\)\\][ \t]*\\'" value)
    (user-error "Invalid tags in the front matter: %s" value))
  (mapcar (lambda (tag)
            (unless (string-match "\\`\"\\([[:alnum:]+-]+\\)\"\\'" tag)
              (user-error "Invalid tag in the front matter: %s" tag))
            (match-string 1 tag))
          (split-string (match-string 1 value) "," t "[ \t]+")))

(defun markdown-note--front-matter (id date title keywords)
  "Return the front matter of the note of ID, DATE, TITLE and KEYWORDS."
  (format "---\ntitle:      %S\ndate:       %s\ntags:       %s\nidentifier: %S\n---\n\n"
          title date (markdown-note--format-tags keywords) id))

(defun markdown-note--goto-value (key)
  "Move point to the value of KEY in the front matter of the buffer.
The front matter is between the --- lines at the beginning."
  (goto-char (point-min))
  (let ((end (and (looking-at "---$")
                  (save-excursion
                    (forward-line 1)
                    (re-search-forward "^---$" nil t)))))
    (unless (and end (re-search-forward (format "^%s[ \t]*:[ \t]*" key) end t))
      (user-error "No %s in the front matter" key))))

(defun markdown-note--value (key)
  "Return the value of KEY in the front matter of the buffer."
  (save-excursion
    (markdown-note--goto-value key)
    (buffer-substring-no-properties (point) (line-end-position))))

(defun markdown-note--set-value (key value)
  "Replace the value of KEY in the front matter of the buffer by VALUE."
  (save-excursion
    (markdown-note--goto-value key)
    (delete-region (point) (line-end-position))
    (insert value)))

;;; new

(defvar-local markdown-note-file nil
  "File of the new note in the buffer.")

(defvar-local markdown-note-window-configuration nil
  "Window configuration before `markdown-note-new'.")

(defun markdown-note-quit ()
  "Kill the buffer of the new note and restore the window configuration."
  (let ((window-configuration markdown-note-window-configuration))
    (kill-buffer (current-buffer))
    (when window-configuration
      (set-window-configuration window-configuration))))

(defun markdown-note-finalize ()
  "Write the buffer to the file of the new note, and visit it."
  (interactive)
  (when (file-exists-p markdown-note-file)
    (user-error "File exists: %s" markdown-note-file))
  (write-file markdown-note-file)
  (markdown-note-new-mode -1)
  (kill-local-variable 'header-line-format))

(defun markdown-note-abort ()
  "Abort the new note."
  (interactive)
  (markdown-note-quit))

(defvar-keymap markdown-note-new-mode-map
  "C-c C-c" #'markdown-note-finalize
  "C-c C-k" #'markdown-note-abort)

(define-minor-mode markdown-note-new-mode
  "Minor mode of the buffer of a new note."
  :lighter " Note")

;;;###autoload
(defun markdown-note-new (title keywords)
  "Show a buffer of a new note of TITLE and KEYWORDS."
  (interactive (list (read-string "Title: ")
                     (split-string (read-string "Keywords: "))))
  (let* ((time (current-time))
         (id (format-time-string "%Y%m%dT%H%M%S" time))
         (keywords (markdown-note--keywords keywords))
         (file (expand-file-name (markdown-note--file-name id title keywords)
                                 markdown-note-directory))
         (window-configuration (current-window-configuration)))
    (pop-to-buffer (generate-new-buffer "*markdown-note*"))
    (funcall markdown-note-markdown-major-mode)
    (markdown-note-new-mode)
    (setq markdown-note-file file
          markdown-note-window-configuration window-configuration)
    (setq header-line-format
          (concat (format "Note %s: " (abbreviate-file-name file))
                  (substitute-command-keys
                   "\\<markdown-note-new-mode-map>\\[markdown-note-finalize] to finish, \\[markdown-note-abort] to abort.")))
    (insert (markdown-note--front-matter
             id (format-time-string "%FT%T%:z" time) title keywords))))

;;; rename

(defun markdown-note--current ()
  "Return (ID TITLE KEYWORDS) of the note of the current buffer."
  (let ((id (and buffer-file-name (markdown-note--file-id buffer-file-name))))
    (unless id
      (user-error "Not a note"))
    (save-restriction
      (widen)
      (list id
            (markdown-note--parse-title (markdown-note--value "title"))
            (markdown-note--parse-tags (markdown-note--value "tags"))))))

;;;###autoload
(defun markdown-note-rename (title keywords)
  "Rename the note of the current buffer by TITLE and KEYWORDS, and save it.
Interactively, read them with the title and tags in the front matter as
the initial input."
  (interactive
   (pcase-let ((`(,_id ,title ,keywords) (markdown-note--current)))
     (list (read-string "Title: " title)
           (split-string (read-string "Keywords: " (string-join keywords " "))))))
  (let* ((id (car (markdown-note--current)))
         (keywords (markdown-note--keywords keywords))
         (file buffer-file-name)
         (new-file (expand-file-name (markdown-note--file-name id title keywords)
                                     (file-name-directory file))))
    (unless (or (string= file new-file) (not (file-exists-p new-file)))
      (user-error "File exists: %s" new-file))
    (save-restriction
      (widen)
      (markdown-note--set-value "title" (format "%S" title))
      (markdown-note--set-value "tags" (markdown-note--format-tags keywords)))
    (unless (string= file new-file)
      (when (file-exists-p file)
        (rename-file file new-file))
      (set-visited-file-name new-file nil t))
    (save-buffer)))

;;; link

(defun markdown-note-read-file-name (&optional prompt)
  "Read the file name of a note with PROMPT, by default \"Note: \"."
  (read-file-name (or prompt "Note: ")
                  (file-name-as-directory markdown-note-directory)
                  nil t nil #'markdown-note--file-id))

(defun markdown-note--parse-link (link)
  "Return the ID of LINK, such as denote:20230730T182013, or nil."
  (and (string-match "\\`denote:\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)\\'" link)
       (match-string 1 link)))

(defun markdown-note-resolve-link (link)
  "Return the file name of the note of LINK, or nil if not a link to a note.
Signal an error if no such note."
  (when-let* ((id (markdown-note--parse-link link)))
    (or (seq-find (lambda (file) (equal (markdown-note--file-id file) id))
                  (directory-files markdown-note-directory t
                                   (concat "\\`" id "--")))
        (user-error "No note: %s" id))))

(defun markdown-note--format-link (id title)
  "Return the link to the note of ID with TITLE as the description."
  (format "[%s](denote:%s)" title id))

;;;###autoload
(defun markdown-note-link (file)
  "Insert a link to the note FILE, with the title in its front matter."
  (interactive (list (markdown-note-read-file-name)))
  (let ((id (or (markdown-note--file-id file) (user-error "Not a note: %s" file)))
        (title (with-temp-buffer
                 (insert-file-contents file)
                 (markdown-note--parse-title (markdown-note--value "title")))))
    (insert (markdown-note--format-link id title))))

;;; backlinks

(defun markdown-note-backlinks-grep (link)
  "Show the lines of LINK in the notes by `lgrep'."
  (require 'grep)
  (grep-compute-defaults)
  (lgrep link "*.md" markdown-note-directory))

(defcustom markdown-note-backlinks-function #'markdown-note-backlinks-grep
  "Function to show the lines of a link, such as denote:ID, in the notes."
  :type 'function)

;;;###autoload
(defun markdown-note-backlinks ()
  "Show the links to the note of the current buffer in the notes.
They are shown by `markdown-note-backlinks-function'."
  (interactive)
  (let ((id (and buffer-file-name (markdown-note--file-id buffer-file-name))))
    (unless id
      (user-error "Not a note"))
    (funcall markdown-note-backlinks-function (concat "denote:" id))))

(provide 'markdown-note)
;;; markdown-note.el ends here
