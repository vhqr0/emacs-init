;;; iselect.el --- Select from candidates incrementally -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: matching, convenience

;;; Commentary:

;; A helm like selection.  (iselect-read PROMPT CANDIDATES &key
;; initial-input history input-fn limit delay layout buffer) reads a
;; candidate in the minibuffer, showing the candidates matching the input
;; in a child frame, or a new frame on text terminals.
;;
;; - CANDIDATES is a list of (STRING . DATA).  STRING is shown and
;;   matched.
;; - The input is a regexp, but spaces match any string, as ".*".  It is
;;   converted by `iselect-regexp-function', and matches ignoring case
;;   unless it has an upper case letter, as isearch and occur.
;; - LIMIT, if non-nil, is the maximum number of matches.  Filtering
;;   stops at it, which keeps large CANDIDATES fast.
;; - DELAY is the seconds to wait after the last command before updating
;;   the matches automatically, `iselect-delay' (0.2) by default.  If
;;   negative, they are not updated automatically.
;; - LAYOUT is `child-frame' or `frame', `iselect-layout' by default, or
;;   nil not to show the candidates.
;; - BUFFER is the buffer of the candidates or its name, "*iselect*" by
;;   default.  It is kept after exiting, to be viewed.
;;
;; In the minibuffer:
;;
;; - The commands moving point, such as `C-n', `C-p', `M-<', `M->',
;;   `C-v', `M-v' and `C-l', select a candidate.
;; - `RET' returns the selected candidate.
;; - `C-M-j' returns the value of INPUT-FN called with the input, or
;;   signals `user-error' without INPUT-FN.
;; - `C-c C-u' updates the candidates, and `C-c C-s' suspends updating
;;   them automatically.
;; - Quitting signals `quit'.
;;
;; The tools built on it:
;;
;; - `iselect-complete' completes the minibuffer input, to be bound in
;;   the maps of `completing-read', such as `icomplete-minibuffer-map'.
;; - `iselect-company' inserts a company candidate, to be bound in
;;   `company-active-map'.
;; - `iselect-history' replaces the input of the minibuffer, Comint or
;;   Eshell with one of its history.  More modes, such as the REPL of
;;   CIDER, can be added to `iselect-history-alist'.
;; - `iselect-files' switches to a buffer, opens a known project in
;;   Dired, jumps to a bookmark, or opens a recent file.
;; - `iselect-project-files' switches to a buffer or opens a file of
;;   the current project, or of a project read by `iselect-project'.
;;
;; The selected candidate is at point in the buffer of the candidates,
;; shown by the cursor of the non-selected window.  To highlight its
;; line and show the line numbers:
;;
;;   (add-hook 'iselect-buffer-mode-hook #'hl-line-mode)
;;   (add-hook 'iselect-buffer-mode-hook #'display-line-numbers-mode)
;;   (add-hook 'iselect-update-hook #'hl-line-highlight)
;;   (add-hook 'iselect-move-hook #'hl-line-highlight)
;;
;; Hl-Line mode updates after the commands in its buffer, while the
;; commands run in the minibuffer, so it is updated by the hooks.  It
;; needs `hl-line-sticky-flag' non-nil, the default, to highlight in
;; the non-selected window.

;;; Code:

;;; iselect-read

(require 'cl-lib)

(defgroup iselect nil
  "Select from candidates incrementally."
  :group 'convenience)

(defface iselect-match
  '((t :inherit match))
  "Face of the parts of candidates matching the input.")

(defvar-local iselect--buffer nil
  "Buffer showing the candidates of the `iselect-read' minibuffer.")

(defvar-local iselect--candidates nil
  "Candidates of the `iselect-read' minibuffer.")

(defvar-local iselect--limit nil
  "Maximum number of the matches of the `iselect-read' minibuffer.")

(defvar-local iselect--last-input nil
  "Input of the shown candidates of the minibuffer, nil if none shown.")

(defvar-local iselect--suspend-p nil
  "Non-nil if updating the candidates of the minibuffer is suspended.")

;;;; match

(defun iselect-regexp-default (input)
  "Return the regexp of INPUT.
INPUT is a regexp, but spaces match any string."
  (string-replace " " ".*" input))

(defcustom iselect-regexp-function #'iselect-regexp-default
  "Function converting the input to the regexp matching the candidates."
  :type 'function)

(defun iselect--case-fold (regexp)
  "Return `case-fold-search' for REGEXP, as isearch and occur do."
  (if (and case-fold-search search-upper-case)
      (isearch-no-upper-case-p regexp t)
    case-fold-search))

(defun iselect--filter (input candidates limit)
  "Return the matches of INPUT in CANDIDATES, at most LIMIT if non-nil.
INPUT is converted to a regexp by `iselect-regexp-function'.  A match is
\(HIGHLIGHTED . CANDIDATE), HIGHLIGHTED being a copy of the string of
CANDIDATE with the matched part highlighted.  Signal `invalid-regexp'
if the regexp is invalid."
  (let* ((regexp (funcall iselect-regexp-function input))
         (case-fold-search (iselect--case-fold regexp))
         matches)
    (while (and candidates (or (null limit) (plusp limit)))
      (let ((candidate (pop candidates)))
        (when (string-match regexp (car candidate))
          (let ((highlighted (copy-sequence (car candidate))))
            (add-face-text-property (match-beginning 0) (match-end 0)
                                    'iselect-match nil highlighted)
            (push (cons highlighted candidate) matches))
          (when limit
            (setq limit (1- limit))))))
    (nreverse matches)))

;;;; render

(defconst iselect--newline
  (propertize " " 'display (propertize "^J" 'face 'escape-glyph))
  "String replacing the newlines of the candidates shown.")

(defun iselect--render (buffer matches)
  "Show MATCHES, as returned by `iselect--filter', in BUFFER.
Each line shows a match, with the newlines shown as \"^J\", and has the
candidate as the property `iselect-candidate'.  Select the first one."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (mapconcat (lambda (match)
                           (propertize (string-replace "\n" iselect--newline (car match))
                                       'iselect-candidate (cdr match)))
                         matches "\n")))
    (goto-char (point-min))
    (let ((window (get-buffer-window buffer t)))
      (when window
        (set-window-point window (point-min))))))

;;;; layout

(defcustom iselect-layout 'child-frame
  "How `iselect-read' shows the buffer of the candidates and the input.
- `child-frame': in a child frame with its own minibuffer, or as
  `frame' on text terminals.
- `frame': in a new frame."
  :type '(choice (const child-frame) (const frame)))

(defcustom iselect-child-frame-parameters
  '((width . 0.62)
    (height . 0.5)
    (child-frame-border-width . 1))
  "Parameters of the child frame.
Float sizes are ratios to the parent frame.  The child frame is centered
in the parent frame."
  :type '(alist :key-type symbol :value-type sexp))

(defcustom iselect-frame-parameters nil
  "Parameters of the new frame."
  :type '(alist :key-type symbol :value-type sexp))

(defun iselect--center-frame (frame)
  "Center FRAME in its parent frame."
  (let ((parent (frame-parent frame)))
    (set-frame-position frame
                        (/ (- (frame-pixel-width parent) (frame-pixel-width frame)) 2)
                        (/ (- (frame-pixel-height parent) (frame-pixel-height frame)) 2))))

(defun iselect--show-frame (buffer parameters)
  "Show BUFFER in a new frame with PARAMETERS, and select it.
Return a function to delete the frame and select the old one."
  (let* ((old-frame (selected-frame))
         (frame (make-frame (append parameters
                                    '((minibuffer . t)
                                      (unsplittable . t)
                                      (no-other-frame . t)
                                      (menu-bar-lines . 0)
                                      (tool-bar-lines . 0)
                                      (tab-bar-lines . 0)))))
         (window (frame-root-window frame)))
    (when (frame-parent frame)
      (iselect--center-frame frame))
    (select-frame-set-input-focus frame)
    (set-window-buffer window buffer)
    (set-window-dedicated-p window t)
    (lambda ()
      (when (frame-live-p old-frame)
        (select-frame-set-input-focus old-frame))
      (when (frame-live-p frame)
        (delete-frame frame)))))

(defun iselect--show (layout buffer)
  "Show BUFFER by LAYOUT, a value of `iselect-layout'.
Return a function to undo it."
  (iselect--show-frame
   buffer
   (if (and (eq layout 'child-frame) (display-graphic-p))
       `(,@iselect-child-frame-parameters
         (parent-frame . ,(selected-frame))
         (undecorated . t))
     iselect-frame-parameters)))

;;;; commands

(defvar iselect-update-hook nil
  "Hook run in the buffer of the candidates after updating its contents.")

(defun iselect-update ()
  "Update the candidates by the input, if changed.
Unlike updating automatically, it is not suspended by
`iselect-toggle-suspend'."
  (interactive)
  (let ((input (minibuffer-contents-no-properties)))
    (unless (equal input iselect--last-input)
      (setq iselect--last-input input)
      ;; Hooks and timers run with `inhibit-quit', so a `C-g' while
      ;; rendering would only set `quit-flag', and quit later
      ;; without exiting.
      (condition-case err
          (let ((inhibit-quit nil))
            (iselect--render iselect--buffer
                             (iselect--filter input iselect--candidates
                                              iselect--limit)))
        ;; The input is often an invalid regexp while typing it.
        (invalid-regexp
         (iselect--render iselect--buffer nil)
         (minibuffer-message "Invalid regexp: %s" (cadr err)))
        (quit (abort-recursive-edit)))
      (with-current-buffer iselect--buffer
        (run-hooks 'iselect-update-hook)))))

(defun iselect--auto-update (minibuffer)
  "Update the candidates of MINIBUFFER unless suspended.
Run by the hook or timer updating them automatically."
  (when (buffer-live-p minibuffer)
    (with-current-buffer minibuffer
      (unless iselect--suspend-p
        (iselect-update)))))

(defun iselect-toggle-suspend ()
  "Toggle suspending updating the candidates automatically.
Resuming updates them at once."
  (interactive)
  (setq iselect--suspend-p (not iselect--suspend-p))
  (unless iselect--suspend-p
    (iselect-update))
  (minibuffer-message (if iselect--suspend-p "[Suspended]" "[Resumed]")))

(defvar iselect-move-hook nil
  "Hook run in the buffer of the candidates after moving point.")

(defun iselect--move (fn)
  "Call FN to move point in the buffer of the candidates.
Then move point to the line start."
  (with-current-buffer iselect--buffer
    (funcall fn)
    (goto-char (line-beginning-position))
    (let ((window (get-buffer-window nil t)))
      (when window
        (set-window-point window (point))))
    (run-hooks 'iselect-move-hook)))

(defun iselect-next (n)
  "Select the Nth next candidate."
  (interactive "p")
  (iselect--move (lambda () (forward-line n))))

(defun iselect-previous (n)
  "Select the Nth previous candidate."
  (interactive "p")
  (iselect-next (- n)))

(defun iselect-first ()
  "Select the first candidate."
  (interactive)
  (iselect--move (lambda () (goto-char (point-min)))))

(defun iselect-last ()
  "Select the last candidate."
  (interactive)
  (iselect--move (lambda () (goto-char (point-max)))))

(defun iselect--scroll (fn)
  "Call FN in the window of the buffer of the candidates, if any.
Then move point to the line start."
  (let ((window (get-buffer-window iselect--buffer t)))
    (when window
      (with-selected-window window
        (funcall fn)
        (goto-char (line-beginning-position))
        (run-hooks 'iselect-move-hook)))))

(defun iselect-scroll-up (&optional arg)
  "Scroll the candidates up, as `scroll-up-command' with ARG."
  (interactive "^P")
  (iselect--scroll (lambda () (scroll-up-command arg))))

(defun iselect-scroll-down (&optional arg)
  "Scroll the candidates down, as `scroll-down-command' with ARG."
  (interactive "^P")
  (iselect--scroll (lambda () (scroll-down-command arg))))

(defun iselect-recenter (&optional arg)
  "Recenter the candidates, as `recenter-top-bottom' with ARG."
  (interactive "P")
  (iselect--scroll (lambda () (recenter-top-bottom arg))))

(defun iselect--candidate ()
  "Return the selected candidate in the current buffer."
  (get-text-property (line-beginning-position) 'iselect-candidate))

(defun iselect--exit (value)
  "Exit the minibuffer, making `iselect-read' return VALUE.
Add the input to the history, as `read-from-minibuffer' does.  Unlike
`exit-minibuffer', it is not affected by the window configuration
restored on exiting, which may move point in the buffer of the
candidates."
  (let ((input (minibuffer-contents-no-properties)))
    (when (and history-add-new-input (not (string-empty-p input)))
      (add-to-history minibuffer-history-variable input)))
  (throw 'iselect--exit value))

(defun iselect-exit ()
  "Exit with the selected candidate."
  (interactive)
  (let ((candidate (with-current-buffer iselect--buffer (iselect--candidate))))
    (unless candidate
      (user-error "No candidate"))
    (iselect--exit (cons 'candidate candidate))))

(defun iselect-exit-input ()
  "Exit with the input, even if no candidate matches it."
  (interactive)
  (iselect--exit (cons 'input (minibuffer-contents-no-properties))))

(defvar-keymap iselect-minibuffer-map
  :parent minibuffer-local-map
  "C-c C-u" #'iselect-update
  "C-c C-s" #'iselect-toggle-suspend
  "<remap> <next-line>" #'iselect-next
  "<remap> <next-line-or-history-element>" #'iselect-next
  "<remap> <previous-line>" #'iselect-previous
  "<remap> <previous-line-or-history-element>" #'iselect-previous
  "<remap> <beginning-of-buffer>" #'iselect-first
  "<remap> <minibuffer-beginning-of-buffer>" #'iselect-first
  "<remap> <end-of-buffer>" #'iselect-last
  "<remap> <scroll-up-command>" #'iselect-scroll-up
  "<remap> <scroll-down-command>" #'iselect-scroll-down
  "<remap> <recenter-top-bottom>" #'iselect-recenter
  "<remap> <exit-minibuffer>" #'iselect-exit
  "C-M-j" #'iselect-exit-input)

;;;; read

(defcustom iselect-delay 0.2
  "Default DELAY of `iselect-read'."
  :type 'number)

(define-derived-mode iselect-buffer-mode special-mode "iselect"
  "Major mode of the buffer showing the candidates."
  (setq truncate-lines t))

(cl-defun iselect-read (prompt candidates &key initial-input history
                               input-fn limit (delay iselect-delay)
                               (layout iselect-layout)
                               buffer)
  "Read a candidate of CANDIDATES with PROMPT, and return it.

CANDIDATES is a list of (STRING . DATA).  STRING is shown, and matched
by the regexp converted from the input by `iselect-regexp-function',
ignoring case unless it has an upper case letter, as isearch and occur.
LIMIT, if non-nil, is the maximum
number of the matches.

\\<iselect-minibuffer-map>DELAY is the seconds to wait after the last command before updating
the matches automatically, `iselect-delay' by default.  If negative,
they are not updated automatically.  \\[iselect-update] updates them at once, and \\[iselect-toggle-suspend]
suspends updating them automatically.

\\[iselect-exit] returns the selected candidate.  \\[iselect-exit-input] returns the value of
INPUT-FN called with the input, or signals `user-error' without
INPUT-FN.  Quitting signals `quit'.  Exiting otherwise, such as by
`exit-recursive-edit', is abnormal, and returns nil.

LAYOUT is how to show the buffer of the candidates, as `iselect-layout',
which is the default.  If nil, the buffer is not shown.  BUFFER is the
buffer of the candidates or its name, \"*iselect*\" by default.  It is
kept after exiting, to be viewed.

INITIAL-INPUT and HISTORY are as in `read-from-minibuffer'.  The input
is added to HISTORY on exiting by \\[iselect-exit] or \\[iselect-exit-input]."
  (let ((buffer (get-buffer-create (or buffer "*iselect*")))
        minibuffer undo-layout timer)
    (with-current-buffer buffer
      (iselect-buffer-mode))
    (pcase (unwind-protect
               (progn
                 (when layout
                   (setq undo-layout (iselect--show layout buffer)))
                 ;; The commands exiting normally throw the result.
                 ;; Exiting without throwing, such as by
                 ;; `exit-recursive-edit', is abnormal, and returns nil.
                 (catch 'iselect--exit
                   (minibuffer-with-setup-hook
                       (lambda ()
                         (setq minibuffer (current-buffer)
                               iselect--buffer buffer
                               iselect--candidates candidates
                               iselect--limit limit)
                         ;; Update by a timer replacing the one of the
                         ;; last command.
                         (unless (minusp delay)
                           (add-hook 'post-command-hook
                                     (lambda ()
                                       (when timer
                                         (cancel-timer timer))
                                       (setq timer (run-with-timer
                                                    delay nil #'iselect--auto-update
                                                    minibuffer)))
                                     nil t))
                         (iselect-update))
                     (read-from-minibuffer
                      prompt initial-input iselect-minibuffer-map nil history))
                   nil))
             (when timer
               (cancel-timer timer))
             (when undo-layout
               (funcall undo-layout)))
      (`(input . ,input)
       (unless input-fn
         (user-error "Input not accepted"))
       (funcall input-fn input))
      (`(candidate . ,candidate) candidate))))

;;; complete

(defvar iselect-complete-history nil
  "History of the inputs of `iselect-complete'.")

(defun iselect-complete ()
  "Complete the minibuffer input with a selected completion.
The completions are of the input in the completion table, sorted by its
`display-sort-function'.  To be bound in the maps of `completing-read'."
  (interactive)
  (let* ((minibuffer (current-buffer))
         (start (minibuffer-prompt-end))
         (input (buffer-substring start (point-max)))
         (completions (completion-all-completions
                       input minibuffer-completion-table
                       minibuffer-completion-predicate (length input)))
         (last (last completions))
         (base (+ start (or (cdr last) 0)))
         (sort (completion-metadata-get
                (completion-metadata input minibuffer-completion-table
                                     minibuffer-completion-predicate)
                'display-sort-function)))
    (unless completions
      (user-error "No completions"))
    (setcdr last nil)
    (when sort
      (setq completions (funcall sort completions)))
    (when-let* ((completion (let ((enable-recursive-minibuffers t))
                              (iselect-read (minibuffer-prompt)
                                            (mapcar (lambda (completion)
                                                      (setq completion (substring-no-properties
                                                                        completion))
                                                      (cons completion completion))
                                                    completions)
                                            :history 'iselect-complete-history
                                            :buffer "*iselect-complete*"))))
      ;; Selecting the frames of iselect may change the current buffer.
      (with-current-buffer minibuffer
        (delete-region base (point-max))
        (goto-char base)
        (insert (cdr completion))))))

;;; company

(defvar company-candidates)
(declare-function company-manual-begin "company")
(declare-function company-finish "company")

(defvar iselect-company-history nil
  "History of the inputs of `iselect-company'.")

(defun iselect-company ()
  "Insert a selected company candidate.
To be bound in `company-active-map'."
  (interactive)
  (when (company-manual-begin)
    (let ((buffer (current-buffer)))
      (when-let* ((candidate (iselect-read "Company: "
                                           (mapcar (lambda (candidate)
                                                     (cons (substring-no-properties candidate)
                                                           candidate))
                                                   company-candidates)
                                           :history 'iselect-company-history
                                           :buffer "*iselect-company*")))
        ;; Selecting the frames of iselect may change the current buffer.
        (with-current-buffer buffer
          (company-finish (cdr candidate)))))))

;;; history

;; The REPLs of SLY and Geiser are derived from `comint-mode'.  For the
;; REPL of CIDER:
;;
;;   (add-to-list 'iselect-history-alist
;;                '(cider-repl-mode cider-repl-input-history
;;                                  cider-repl-input-history-position
;;                                  cider-repl-input-start-mark))

(defcustom iselect-history-alist
  '((eshell-mode eshell-history-ring eshell-history-index beginning-of-line)
    (comint-mode comint-input-ring comint-input-ring-index comint-bol))
  "Alist of the histories of modes for `iselect-history'.
Each element is (MODE HISTORY INDEX BOL).  HISTORY is a variable of a
list or a ring of the inputs, the latest first.  INDEX, if non-nil, is
a variable set to the index of the input selected in HISTORY.  BOL is a
function moving point to the beginning of the input, or a variable of
a marker at it.  The first element whose MODE the current mode derives
from is used."
  :type '(alist :key-type symbol
                :value-type (list (variable :tag "History")
                                  (choice (const :tag "No index" nil)
                                          (variable :tag "Index"))
                                  (choice (function :tag "Function")
                                          (variable :tag "Marker"))))
  :group 'iselect)

(defcustom iselect-history-limit 200
  "Maximum number of the matches of `iselect-history'."
  :type '(choice (const :tag "No limit" nil) natnum)
  :group 'iselect)

(declare-function ring-elements "ring")

(defun iselect--history ()
  "Return (HISTORY INDEX BEG END) of the current buffer.
HISTORY is a list or a ring, and INDEX is as in `iselect-history-alist'.
The input is between BEG and END."
  (if (minibufferp)
      (progn
        (when (eq minibuffer-history-variable t)
          (user-error "No history"))
        (list (symbol-value minibuffer-history-variable)
              nil (minibuffer-prompt-end) (point-max)))
    (pcase (seq-find (lambda (entry)
                       (and (derived-mode-p (car entry))
                            (boundp (nth 1 entry))))
                     iselect-history-alist)
      ('nil (user-error "No history of %s" major-mode))
      (`(,_ ,history ,index ,bol)
       (save-excursion
         (if (functionp bol)
             (funcall bol)
           (goto-char (symbol-value bol)))
         (list (symbol-value history) index (point) (pos-eol)))))))

(defun iselect-history ()
  "Replace the input with a selected one of the history.
In the minibuffer, the history is of `minibuffer-history-variable'.
Otherwise, it is of the current mode in `iselect-history-alist'."
  (interactive)
  (pcase-let* ((`(,history ,index ,beg ,end) (iselect--history))
               (history (if (ring-p history) (ring-elements history) history))
               (buffer (current-buffer))
               ;; The process output may move the input while reading.
               (beg (copy-marker beg))
               (end (copy-marker end t)))
    (unless history
      (user-error "No history"))
    (when-let* ((candidate (let ((enable-recursive-minibuffers t))
                             (iselect-read "History: "
                                           (mapcar (lambda (input)
                                                     (cons (substring-no-properties input)
                                                           input))
                                                   (delete-dups (copy-sequence history)))
                                           :initial-input (regexp-quote
                                                           (buffer-substring-no-properties
                                                            beg end))
                                           :limit iselect-history-limit
                                           :history t
                                           :buffer "*iselect-history*"))))
      ;; Selecting the frames of iselect may change the current buffer.
      (with-current-buffer buffer
        (delete-region beg end)
        (goto-char beg)
        (insert (substring-no-properties (cdr candidate)))
        (when index
          (set index (seq-position history (cdr candidate))))))))

;;; files

;; The data of the candidates of the files commands are (TYPE . VALUE):
;; (buffer . BUFFER-OR-NAME), (project . ROOT), (bookmark . NAME), or
;; (file . FILE).

(defvar recentf-list)
(declare-function bookmark-all-names "bookmark")
(declare-function project-root "project")
(declare-function project-files "project")
(declare-function project-buffers "project")
(declare-function project-known-project-roots "project")

(defcustom iselect-files-limit 200
  "Maximum number of the matches of the files commands."
  :type '(choice (const :tag "No limit" nil) natnum)
  :group 'iselect)

(defface iselect-buffer
  '((t :inherit font-lock-type-face))
  "Face of the buffer candidates."
  :group 'iselect)

(defface iselect-bookmark
  '((t :inherit font-lock-keyword-face))
  "Face of the bookmark candidates."
  :group 'iselect)

(defface iselect-project
  '((t :inherit font-lock-constant-face))
  "Face of the project candidates."
  :group 'iselect)

(defun iselect--open (prompt candidates &rest args)
  "Read a candidate of CANDIDATES with PROMPT, and open it by its type.
The data of CANDIDATES are (TYPE . VALUE).  ARGS are passed to
`iselect-read'."
  (pcase (cdr (apply #'iselect-read prompt candidates args))
    (`(buffer . ,buffer) (switch-to-buffer buffer))
    (`(project . ,root) (find-file root))
    (`(bookmark . ,name) (bookmark-jump name))
    (`(file . ,file) (find-file file))))

;;;; candidates

(defun iselect--buffer-candidates (buffers)
  "Return the candidates of BUFFERS, except the hidden ones.
The current buffer is moved last."
  (let* ((current (current-buffer))
         (buffers (seq-remove (lambda (buffer)
                                (string-prefix-p " " (buffer-name buffer)))
                              buffers)))
    (when (memq current buffers)
      (setq buffers (append (delq current buffers) (list current))))
    (mapcar (lambda (buffer)
              (cons (propertize (buffer-name buffer) 'face 'iselect-buffer)
                    (cons 'buffer buffer)))
            buffers)))

(defun iselect--project-candidates ()
  "Return the candidates of the known projects."
  (require 'project)
  (mapcar (lambda (root)
            (cons (propertize (abbreviate-file-name root) 'face 'iselect-project)
                  (cons 'project root)))
          (project-known-project-roots)))

(defun iselect--bookmark-candidates ()
  "Return the candidates of the bookmarks."
  (require 'bookmark)
  (mapcar (lambda (name)
            (cons (propertize name 'face 'iselect-bookmark)
                  (cons 'bookmark name)))
          (bookmark-all-names)))

(defun iselect--recentf-candidates ()
  "Return the candidates of the recent files."
  (require 'recentf)
  (mapcar (lambda (file)
            (cons (abbreviate-file-name file) (cons 'file file)))
          recentf-list))

;;;; files

(defvar iselect-files-history nil
  "History of the inputs of `iselect-files'.")

(defun iselect-files ()
  "Switch to a buffer, open a project or a file, or jump to a bookmark.
The candidates are the buffers, the known projects, the bookmarks, and
the recent files.  A project is opened in Dired.
\\<iselect-minibuffer-map>\\[iselect-exit-input] switches to the buffer named by the input,
creating it if needed."
  (interactive)
  (iselect--open "Files: "
                 (append (iselect--buffer-candidates (buffer-list))
                         (iselect--project-candidates)
                         (iselect--bookmark-candidates)
                         (iselect--recentf-candidates))
                 :limit iselect-files-limit
                 :history 'iselect-files-history
                 :buffer "*iselect-files*"
                 :input-fn (lambda (input)
                             (cons input (cons 'buffer input)))))

;;;; project files

(defvar iselect-project-history nil
  "History of the inputs of `iselect-project'.")

(defun iselect-project ()
  "Read a known project, and return its root, or nil if none read."
  (require 'project)
  (when-let* ((candidate (iselect-read "Project: "
                                       (mapcar (lambda (root)
                                                 (cons (abbreviate-file-name root) root))
                                               (project-known-project-roots))
                                       :limit iselect-files-limit
                                       :history 'iselect-project-history
                                       :buffer "*iselect-project*")))
    (cdr candidate)))

(defvar iselect-project-files-history nil
  "History of the inputs of `iselect-project-files'.")

(defun iselect-project-files (dir)
  "Switch to a buffer or open a file of the project of DIR.
Do nothing if DIR is nil.  Interactively, DIR is `default-directory', or
read by `iselect-project' with a prefix argument.
\\<iselect-minibuffer-map>\\[iselect-exit-input] opens the file named by the input, relative to
the project root."
  (interactive (list (if current-prefix-arg
                         (iselect-project)
                       default-directory)))
  ;; Not binding `default-directory', which is local to the current
  ;; buffer, and would move it to the project of DIR.
  (when dir
    (let* ((project (project-current t dir))
           (root (expand-file-name (project-root project))))
      (iselect--open (format "Project %s: " (abbreviate-file-name root))
                     (append (iselect--buffer-candidates (project-buffers project))
                             (mapcar (lambda (file)
                                       (cons (if (string-prefix-p root file)
                                                 (substring file (length root))
                                               file)
                                             (cons 'file file)))
                                     (project-files project)))
                     :limit iselect-files-limit
                     :history 'iselect-project-files-history
                     :buffer "*iselect-project-files*"
                     :input-fn (lambda (input)
                                 (cons input
                                       (cons 'file (expand-file-name input root))))))))

(provide 'iselect)
;;; iselect.el ends here
