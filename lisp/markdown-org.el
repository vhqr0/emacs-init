;;; markdown-org.el --- Org-like extensions for markdown -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1") (yasnippet "0.14"))
;; Version: 0.1.0
;; Keywords: outlines, convenience

;;; Commentary:

;; Org-like extensions for markdown.
;;
;; `markdown-org-toggle-todo' toggles "TODO " after the # marks of the
;; heading at the current line.
;;
;; `markdown-org-capture' reads a char, then captures a note by the template
;; of it in `markdown-org-capture-templates', a list of (CHAR DESCRIPTION
;; FILE TEMPLATE).  TEMPLATE is a yasnippet snippet, expanded literally,
;; without reindenting, in a temporary buffer of
;; `markdown-org-markdown-major-mode', `text-mode' by default.  `C-c C-c'
;; appends the buffer to FILE, and `C-c C-k' aborts.
;;
;;   (setq markdown-org-markdown-major-mode #'markdown-mode)
;;   (setq markdown-org-capture-templates
;;         '((?t "Todo" "~/notes/inbox.md"
;;               "## TODO $0\n`(format-time-string \"[%F %a %R]\")`\n")))
;;
;; Without templates, it captures to `markdown-org-capture-default-file' by
;; `markdown-org-capture-default-template': a TODO heading, the current
;; time, the file and line where capture started, and the text of the
;; line in a code block.
;;
;; Templates may use these variables in backquoted elisp, describing
;; where capture started:
;;
;; - `markdown-org-capture-origin-buffer': the buffer.
;; - `markdown-org-capture-origin-file': its file name, or nil.
;; - `markdown-org-capture-origin-line': the line number.
;; - `markdown-org-capture-origin-line-text': the text of the line.
;; - `markdown-org-capture-origin-region': the text of the active region,
;;   or nil.
;;
;;   (?a "Todo with link" "~/notes/inbox.md"
;;       "## TODO $0\n[`markdown-org-capture-origin-line-text`](`markdown-org-capture-origin-file`)\n")
;;
;; `markdown-org-agenda-files' lists files or directories, a directory
;; standing for the markdown files in it, defaulting to
;; `markdown-org-capture-default-file'.  `markdown-org-agenda-headings'
;; collects their headings as (FILE LINE TEXT), skipping fenced code
;; blocks.  `markdown-org-agenda-todo' lists the TODO headings, with
;; the # marks highlighted by `markdown-org-heading-marks', and TODO by
;; `markdown-org-todo':
;;
;; - `RET' goes to the heading in another window, and `M-RET' displays
;;   it, keeping the selected window.  `M-n' and `M-p' move to the next
;;   and previous entries, and display them.
;; - `C-c C-t' toggles TODO of the heading.
;; - `g' regenerates the list.

;;; Code:

(declare-function yas-expand-snippet "yasnippet")
(declare-function yas-minor-mode "yasnippet")

(defvar markdown-org-markdown-major-mode #'text-mode
  "Major mode for markdown, such as `markdown-mode'.")

(defconst markdown-org-heading-regexp "^#+[ \t]+"
  "Regexp matching the beginning of a heading.")

(defconst markdown-org-todo-regexp (concat markdown-org-heading-regexp "TODO ")
  "Regexp matching the beginning of a TODO heading.")

;;; todo

;;;###autoload
(defun markdown-org-toggle-todo ()
  "Toggle TODO of the heading at the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at markdown-org-heading-regexp)
      (user-error "Not at a heading"))
    (goto-char (match-end 0))
    (if (looking-at "TODO ")
        (delete-region (match-beginning 0) (match-end 0))
      (insert "TODO "))))

;;; capture

(defvar markdown-org-capture-templates nil
  "List of (CHAR DESCRIPTION FILE TEMPLATE) for `markdown-org-capture'.
CHAR is the key to select the template.  TEMPLATE is a yasnippet
snippet, expanded and then appended to FILE.")

(defvar markdown-org-capture-default-file (locate-user-emacs-file "notes.md")
  "Default file to capture to and collect agenda headings from.")

(defvar markdown-org-capture-default-template
  "# TODO $0
`(format-time-string \"%F %a %R\")`
`(or markdown-org-capture-origin-file (buffer-name markdown-org-capture-origin-buffer))` `markdown-org-capture-origin-line`
\\`\\`\\`
`markdown-org-capture-origin-line-text`
\\`\\`\\`
"
  "Default capture template, used when `markdown-org-capture-templates' is nil.")

(defvar-local markdown-org-capture-file nil
  "File to append the capture buffer to.")

(defvar-local markdown-org-capture-window-configuration nil
  "Window configuration before capture.")

(defvar-local markdown-org-capture-origin-buffer nil
  "Buffer where capture started, for use in templates.")

(defvar-local markdown-org-capture-origin-file nil
  "File name of `markdown-org-capture-origin-buffer', for use in templates.")

(defvar-local markdown-org-capture-origin-line nil
  "Line number where capture started, for use in templates.")

(defvar-local markdown-org-capture-origin-line-text nil
  "Text of the line where capture started, for use in templates.")

(defvar-local markdown-org-capture-origin-region nil
  "Text of the active region where capture started, for use in templates.")

(defun markdown-org-capture-read-template ()
  "Read a char and return the capture template of it.
Return the default template if `markdown-org-capture-templates' is nil."
  (if (null markdown-org-capture-templates)
      (list nil "Default" markdown-org-capture-default-file markdown-org-capture-default-template)
    (let* ((prompt (mapconcat (lambda (template)
                                (format "[%c] %s" (nth 0 template) (nth 1 template)))
                              markdown-org-capture-templates
                              " "))
           (char (read-char-choice (concat prompt ": ")
                                   (mapcar #'car markdown-org-capture-templates))))
      (assq char markdown-org-capture-templates))))

(defun markdown-org-capture-quit ()
  "Kill the capture buffer and restore the window configuration."
  (let ((window-configuration markdown-org-capture-window-configuration))
    (kill-buffer (current-buffer))
    (when window-configuration
      (set-window-configuration window-configuration))))

(defun markdown-org-capture-finalize ()
  "Append the capture buffer to its file and quit."
  (interactive)
  (let ((content (buffer-string))
        (file markdown-org-capture-file))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-max))
          (unless (bolp)
            (insert "\n"))
          (insert content)
          (unless (bolp)
            (insert "\n"))))
      (save-buffer))
    (markdown-org-capture-quit)))

(defun markdown-org-capture-abort ()
  "Abort capture."
  (interactive)
  (markdown-org-capture-quit))

(defvar-keymap markdown-org-capture-mode-map
  "C-c C-c" #'markdown-org-capture-finalize
  "C-c C-k" #'markdown-org-capture-abort)

(define-minor-mode markdown-org-capture-mode
  "Minor mode of capture buffers."
  :lighter " Capture")

;;;###autoload
(defun markdown-org-capture ()
  "Capture a note by a template of `markdown-org-capture-templates'."
  (interactive)
  (require 'yasnippet)
  (pcase-let* ((`(,_char ,_description ,file ,template) (markdown-org-capture-read-template))
               (window-configuration (current-window-configuration))
               (file (expand-file-name file))
               (origin-buffer (current-buffer))
               (origin-file buffer-file-name)
               (origin-line (line-number-at-pos))
               (origin-line-text (buffer-substring-no-properties
                                  (line-beginning-position) (line-end-position)))
               (origin-region (and (use-region-p)
                                   (buffer-substring-no-properties
                                    (region-beginning) (region-end))))
               (buffer (generate-new-buffer "*markdown-org-capture*")))
    (pop-to-buffer buffer)
    (funcall markdown-org-markdown-major-mode)
    (markdown-org-capture-mode 1)
    (setq markdown-org-capture-file file
          markdown-org-capture-window-configuration window-configuration
          markdown-org-capture-origin-buffer origin-buffer
          markdown-org-capture-origin-file origin-file
          markdown-org-capture-origin-line origin-line
          markdown-org-capture-origin-line-text origin-line-text
          markdown-org-capture-origin-region origin-region)
    (setq header-line-format
          (concat (format "Capture to %s: " (abbreviate-file-name file))
                  (substitute-command-keys
                   "\\<markdown-org-capture-mode-map>\\[markdown-org-capture-finalize] to finish, \\[markdown-org-capture-abort] to abort.")))
    (yas-minor-mode 1)
    (yas-expand-snippet template nil nil '((yas-indent-line nil)))))

;;; agenda

(defvar markdown-org-agenda-files nil
  "List of files or directories to collect agenda headings from.
A directory stands for the markdown files in it.  If nil, only
`markdown-org-capture-default-file'.")

(defun markdown-org-agenda-file-list ()
  "Return the markdown files of `markdown-org-agenda-files'."
  (mapcan (lambda (file)
            (let ((file (expand-file-name file)))
              (if (file-directory-p file)
                  (directory-files file t "\\.md\\'")
                (list file))))
          (or markdown-org-agenda-files (list markdown-org-capture-default-file))))

(defun markdown-org-agenda-file-headings (file)
  "Return the headings of FILE as (FILE LINE TEXT).
Lines in fenced code blocks are skipped."
  (with-temp-buffer
    (if-let* ((buffer (get-file-buffer file)))
        (insert-buffer-substring buffer)
      (insert-file-contents file))
    (goto-char (point-min))
    (let ((line 1) fence headings)
      (while (not (eobp))
        (cond
         ((looking-at "^[ \t]*\\(```\\|~~~\\)")
          (let ((marker (match-string 1)))
            (cond ((null fence) (setq fence marker))
                  ((equal fence marker) (setq fence nil)))))
         ((and (null fence) (looking-at markdown-org-heading-regexp))
          (push (list file line (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position)))
                headings)))
        (forward-line 1)
        (setq line (1+ line)))
      (nreverse headings))))

(defun markdown-org-agenda-headings ()
  "Return the headings of `markdown-org-agenda-files' as (FILE LINE TEXT)."
  (mapcan #'markdown-org-agenda-file-headings (markdown-org-agenda-file-list)))

(defun markdown-org-agenda-heading ()
  "Return the (FILE LINE TEXT) heading of the agenda entry at point."
  (or (get-text-property (line-beginning-position) 'markdown-org-agenda-heading)
      (user-error "No agenda entry at point")))

(defun markdown-org-agenda-insert-heading (heading)
  "Insert an agenda entry of HEADING, a (FILE LINE TEXT)."
  (insert (propertize (concat (nth 2 heading) "\n")
                      'markdown-org-agenda-heading heading)))

(defun markdown-org-agenda-visit (heading)
  "Go to the line of HEADING, a (FILE LINE TEXT)."
  (goto-char (point-min))
  (forward-line (1- (nth 1 heading))))

(defun markdown-org-agenda-goto (&optional display)
  "Go to the heading of the agenda entry at point in another window.
If DISPLAY is non-nil, only display it, keeping the selected window."
  (interactive)
  (let* ((heading (markdown-org-agenda-heading))
         (buffer (find-file-noselect (nth 0 heading)))
         (window (if display
                     (display-buffer buffer t)
                   (switch-to-buffer-other-window buffer)
                   (selected-window))))
    (with-selected-window window
      (markdown-org-agenda-visit heading))))

(defun markdown-org-agenda-display ()
  "Display the heading of the agenda entry at point in another window."
  (interactive)
  (markdown-org-agenda-goto t))

(defun markdown-org-agenda-display-next (&optional n)
  "Move to the Nth next agenda entry, and display its heading."
  (interactive "p")
  (forward-line (or n 1))
  ;; Not after the last entry, at the empty last line.
  (when (and (eobp) (not (bobp)))
    (forward-line -1))
  (markdown-org-agenda-display))

(defun markdown-org-agenda-display-previous (&optional n)
  "Move to the Nth previous agenda entry, and display its heading."
  (interactive "p")
  (markdown-org-agenda-display-next (- (or n 1))))

(defun markdown-org-agenda-toggle-todo ()
  "Toggle TODO of the heading of the agenda entry at point.
Signal an error if the heading no longer matches its file."
  (interactive)
  (pcase-let* ((`(,file ,line ,text) (markdown-org-agenda-heading))
               (new-text
                (with-current-buffer (find-file-noselect file)
                  (save-excursion
                    (save-restriction
                      (widen)
                      (markdown-org-agenda-visit (list file line text))
                      (unless (equal (buffer-substring-no-properties
                                      (line-beginning-position) (line-end-position))
                                     text)
                        (user-error "Heading changed in %s, revert the agenda" file))
                      (markdown-org-toggle-todo)
                      (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))))
               (inhibit-read-only t))
    (beginning-of-line)
    (delete-region (point) (line-beginning-position 2))
    (save-excursion
      (markdown-org-agenda-insert-heading (list file line new-text)))))

(defvar-keymap markdown-org-agenda-mode-map
  "RET" #'markdown-org-agenda-goto
  "M-RET" #'markdown-org-agenda-display
  "M-n" #'markdown-org-agenda-display-next
  "M-p" #'markdown-org-agenda-display-previous
  "C-c C-t" #'markdown-org-agenda-toggle-todo)

(defgroup markdown-org nil
  "Org-like extensions for markdown."
  :group 'text)

(defface markdown-org-heading-marks '((t :inherit font-lock-keyword-face))
  "Face of the # marks of the headings in markdown agenda buffers.")

(defface markdown-org-todo '((t :inherit font-lock-warning-face))
  "Face of TODO of the headings in markdown agenda buffers.")

(defvar markdown-org-agenda-font-lock-keywords
  '(("^\\(#+\\)[ \t]+\\(?:\\(TODO\\) \\)?"
     (1 'markdown-org-heading-marks)
     (2 'markdown-org-todo nil t)))
  "Font lock keywords of markdown agenda buffers.")

(define-derived-mode markdown-org-agenda-mode special-mode "MD-Agenda"
  "Major mode of markdown agenda buffers."
  (setq-local font-lock-defaults '(markdown-org-agenda-font-lock-keywords t)))

(defun markdown-org-agenda-todo-insert ()
  "Insert the TODO headings of `markdown-org-agenda-files'."
  (dolist (heading (markdown-org-agenda-headings))
    (when (string-match-p markdown-org-todo-regexp (nth 2 heading))
      (markdown-org-agenda-insert-heading heading))))

(defun markdown-org-agenda-todo-revert (&rest _)
  "Regenerate the TODO agenda buffer."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos)))
    (erase-buffer)
    (markdown-org-agenda-todo-insert)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun markdown-org-agenda-todo ()
  "List the TODO headings of `markdown-org-agenda-files'."
  (interactive)
  (let ((directory default-directory))
    (with-current-buffer (get-buffer-create "*markdown-org-agenda-todo*")
      (markdown-org-agenda-mode)
      (setq default-directory directory)
      (setq-local revert-buffer-function #'markdown-org-agenda-todo-revert)
      (revert-buffer)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(provide 'markdown-org)
;;; markdown-org.el ends here
