;;; markdown-x.el --- Org-like extensions for markdown -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1") (yasnippet "0.14"))
;; Version: 0.1.0
;; Keywords: outlines, convenience

;;; Commentary:

;; Org-like extensions for markdown:
;; - `markdown-x-toggle-todo' toggles TODO of a heading.
;; - `markdown-x-capture' captures notes by `markdown-x-capture-templates'.
;; - `markdown-x-agenda-todo' lists TODO headings of
;;   `markdown-x-agenda-files'.

;;; Code:

(declare-function yas-expand-snippet "yasnippet")
(declare-function yas-minor-mode "yasnippet")

(defconst markdown-x-heading-regexp "^#+[ \t]+"
  "Regexp matching the beginning of a heading.")

(defconst markdown-x-todo-regexp (concat markdown-x-heading-regexp "TODO ")
  "Regexp matching the beginning of a TODO heading.")

;;; todo

;;;###autoload
(defun markdown-x-toggle-todo ()
  "Toggle TODO of the heading at the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at markdown-x-heading-regexp)
      (user-error "Not at a heading"))
    (goto-char (match-end 0))
    (if (looking-at "TODO ")
        (delete-region (match-beginning 0) (match-end 0))
      (insert "TODO "))))

;;; capture

(defvar markdown-x-capture-templates nil
  "List of (CHAR DESCRIPTION FILE TEMPLATE) for `markdown-x-capture'.
CHAR is the key to select the template.  TEMPLATE is a yasnippet
snippet, expanded and then appended to FILE.")

(defvar markdown-x-capture-default-file (locate-user-emacs-file "notes.md")
  "Default file to capture to and collect agenda headings from.")

(defvar markdown-x-capture-default-template
  "# TODO $0
`(format-time-string \"%F %a %R\")`
`(or markdown-x-capture-origin-file (buffer-name markdown-x-capture-origin-buffer))` `markdown-x-capture-origin-line`
    `markdown-x-capture-origin-line-text`
"
  "Default capture template, used when `markdown-x-capture-templates' is nil.")

(defvar markdown-x-capture-major-mode #'text-mode
  "Major mode of capture buffers, such as `markdown-mode'.")

(defvar-local markdown-x-capture-file nil
  "File to append the capture buffer to.")

(defvar-local markdown-x-capture-window-configuration nil
  "Window configuration before capture.")

(defvar-local markdown-x-capture-origin-buffer nil
  "Buffer where capture started, for use in templates.")

(defvar-local markdown-x-capture-origin-file nil
  "File name of `markdown-x-capture-origin-buffer', for use in templates.")

(defvar-local markdown-x-capture-origin-line nil
  "Line number where capture started, for use in templates.")

(defvar-local markdown-x-capture-origin-line-text nil
  "Text of the line where capture started, for use in templates.")

(defvar-local markdown-x-capture-origin-region nil
  "Text of the active region where capture started, for use in templates.")

(defun markdown-x-capture-read-template ()
  "Read a char and return the capture template of it.
Return the default template if `markdown-x-capture-templates' is nil."
  (if (null markdown-x-capture-templates)
      (list nil "Default" markdown-x-capture-default-file markdown-x-capture-default-template)
    (let* ((prompt (mapconcat (lambda (template)
                                (format "[%c] %s" (nth 0 template) (nth 1 template)))
                              markdown-x-capture-templates
                              " "))
           (char (read-char-choice (concat prompt ": ")
                                   (mapcar #'car markdown-x-capture-templates))))
      (assq char markdown-x-capture-templates))))

(defun markdown-x-capture-quit ()
  "Kill the capture buffer and restore the window configuration."
  (let ((window-configuration markdown-x-capture-window-configuration))
    (kill-buffer (current-buffer))
    (when window-configuration
      (set-window-configuration window-configuration))))

(defun markdown-x-capture-finalize ()
  "Append the capture buffer to its file and quit."
  (interactive)
  (let ((content (buffer-string))
        (file markdown-x-capture-file))
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
    (markdown-x-capture-quit)))

(defun markdown-x-capture-abort ()
  "Abort capture."
  (interactive)
  (markdown-x-capture-quit))

(defvar-keymap markdown-x-capture-mode-map
  "C-c C-c" #'markdown-x-capture-finalize
  "C-c C-k" #'markdown-x-capture-abort)

(define-minor-mode markdown-x-capture-mode
  "Minor mode of capture buffers."
  :lighter " Capture")

;;;###autoload
(defun markdown-x-capture ()
  "Capture a note by a template of `markdown-x-capture-templates'."
  (interactive)
  (require 'yasnippet)
  (pcase-let* ((`(,_char ,_description ,file ,template) (markdown-x-capture-read-template))
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
               (buffer (generate-new-buffer "*markdown-x-capture*")))
    (pop-to-buffer buffer)
    (funcall markdown-x-capture-major-mode)
    (markdown-x-capture-mode 1)
    (setq markdown-x-capture-file file
          markdown-x-capture-window-configuration window-configuration
          markdown-x-capture-origin-buffer origin-buffer
          markdown-x-capture-origin-file origin-file
          markdown-x-capture-origin-line origin-line
          markdown-x-capture-origin-line-text origin-line-text
          markdown-x-capture-origin-region origin-region)
    (setq header-line-format
          (concat (format "Capture to %s: " (abbreviate-file-name file))
                  (substitute-command-keys
                   "\\<markdown-x-capture-mode-map>\\[markdown-x-capture-finalize] to finish, \\[markdown-x-capture-abort] to abort.")))
    (yas-minor-mode 1)
    (yas-expand-snippet template nil nil '((yas-indent-line nil)))))

;;; agenda

(defvar markdown-x-agenda-files nil
  "List of files or directories to collect agenda headings from.
A directory stands for the markdown files in it.  If nil, only
`markdown-x-capture-default-file'.")

(defun markdown-x-agenda-file-list ()
  "Return the markdown files of `markdown-x-agenda-files'."
  (mapcan (lambda (file)
            (let ((file (expand-file-name file)))
              (if (file-directory-p file)
                  (directory-files file t "\\.md\\'")
                (list file))))
          (or markdown-x-agenda-files (list markdown-x-capture-default-file))))

(defun markdown-x-agenda-file-headings (file)
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
         ((and (null fence) (looking-at markdown-x-heading-regexp))
          (push (list file line (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position)))
                headings)))
        (forward-line 1)
        (setq line (1+ line)))
      (nreverse headings))))

(defun markdown-x-agenda-headings ()
  "Return the headings of `markdown-x-agenda-files' as (FILE LINE TEXT)."
  (mapcan #'markdown-x-agenda-file-headings (markdown-x-agenda-file-list)))

(defun markdown-x-agenda-heading ()
  "Return the (FILE LINE TEXT) heading of the agenda entry at point."
  (or (get-text-property (line-beginning-position) 'markdown-x-agenda-heading)
      (user-error "No agenda entry at point")))

(defun markdown-x-agenda-insert-heading (heading)
  "Insert an agenda entry of HEADING, a (FILE LINE TEXT)."
  (insert (propertize (concat (nth 2 heading) "\n")
                      'markdown-x-agenda-heading heading)))

(defun markdown-x-agenda-visit (heading)
  "Go to the line of HEADING, a (FILE LINE TEXT)."
  (goto-char (point-min))
  (forward-line (1- (nth 1 heading))))

(defun markdown-x-agenda-goto ()
  "Go to the heading of the agenda entry at point."
  (interactive)
  (let ((heading (markdown-x-agenda-heading)))
    (find-file (nth 0 heading))
    (markdown-x-agenda-visit heading)))

(defun markdown-x-agenda-goto-other-window ()
  "Go to the heading of the agenda entry at point in other window."
  (interactive)
  (let ((heading (markdown-x-agenda-heading)))
    (find-file-other-window (nth 0 heading))
    (markdown-x-agenda-visit heading)))

(defun markdown-x-agenda-toggle-todo ()
  "Toggle TODO of the heading of the agenda entry at point.
Signal an error if the heading no longer matches its file."
  (interactive)
  (pcase-let* ((`(,file ,line ,text) (markdown-x-agenda-heading))
               (new-text
                (with-current-buffer (find-file-noselect file)
                  (save-excursion
                    (save-restriction
                      (widen)
                      (markdown-x-agenda-visit (list file line text))
                      (unless (equal (buffer-substring-no-properties
                                      (line-beginning-position) (line-end-position))
                                     text)
                        (user-error "Heading changed in %s, revert the agenda" file))
                      (markdown-x-toggle-todo)
                      (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))))
               (inhibit-read-only t))
    (beginning-of-line)
    (delete-region (point) (line-beginning-position 2))
    (save-excursion
      (markdown-x-agenda-insert-heading (list file line new-text)))))

(defvar-keymap markdown-x-agenda-mode-map
  "RET" #'markdown-x-agenda-goto
  "o" #'markdown-x-agenda-goto-other-window
  "C-c C-t" #'markdown-x-agenda-toggle-todo)

(define-derived-mode markdown-x-agenda-mode special-mode "MD-Agenda"
  "Major mode of markdown agenda buffers.")

(defun markdown-x-agenda-todo-insert ()
  "Insert the TODO headings of `markdown-x-agenda-files'."
  (dolist (heading (markdown-x-agenda-headings))
    (when (string-match-p markdown-x-todo-regexp (nth 2 heading))
      (markdown-x-agenda-insert-heading heading))))

(defun markdown-x-agenda-todo-revert (&rest _)
  "Regenerate the TODO agenda buffer."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos)))
    (erase-buffer)
    (markdown-x-agenda-todo-insert)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun markdown-x-agenda-todo ()
  "List the TODO headings of `markdown-x-agenda-files'."
  (interactive)
  (let ((directory default-directory))
    (with-current-buffer (get-buffer-create "*markdown-x-agenda-todo*")
      (markdown-x-agenda-mode)
      (setq default-directory directory)
      (setq-local revert-buffer-function #'markdown-x-agenda-todo-revert)
      (revert-buffer)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(provide 'markdown-x)
;;; markdown-x.el ends here
