;;; mygreper.el --- Search files by ripgrep in the minibuffer with preview -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: matching

;;; Commentary:

;; `mygreper' searches the files in `default-directory' by ripgrep,
;; lists the lines matching the input under the input in the
;; minibuffer, and previews the selected one in the window.  To search
;; another directory, such as the root of the current project, bind
;; `default-directory'.
;;
;; In the minibuffer:
;;
;; - `C-n', `C-p', `M-<', `M->', `C-v' and `M-v' select a line.
;; - `RET' visits the selected line, and `C-g' goes back.
;; - `C-l' recenters the window of the search.
;; - `C-x C-s' runs the search in a `grep-mode' buffer, and selects it.
;;
;; The input is a regexp of ripgrep, not of Emacs, converted by
;; `mygreper-regexp-function', by default spaces match any string.  The
;; matches are found and colored by ripgrep only.  The search ignores
;; case unless the input has an upper case letter, by `--smart-case' of
;; ripgrep, overridden by `--case-sensitive' or `--ignore-case' in
;; `mygreper-args'.  The search runs `mygreper-delay' seconds after the
;; last input of at least `mygreper-min-input' chars, and stops after
;; `mygreper-limit' lines.  The lines found are shown while searching,
;; at most every `mygreper-render-interval' seconds.
;;
;;   (setq mygreper-args '("--hidden" "--glob=!.git/"))
;;
;; The window previewing moves to the first match of the selected line.
;; To highlight the line by `hl-line-mode':
;;
;;   (add-hook 'mygreper-move-hook #'hl-line-highlight)

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'ansi-color)
(require 'grep)
(require 'xref)

(defgroup mygreper nil
  "Search files by ripgrep in the minibuffer with preview."
  :group 'matching)

(defcustom mygreper-height 10
  "Number of the lines shown in the minibuffer."
  :type 'natnum)

(defcustom mygreper-program "rg"
  "Program of ripgrep."
  :type 'string)

(defcustom mygreper-args nil
  "Additional arguments of ripgrep."
  :type '(repeat string))

(defcustom mygreper-limit 500
  "Number of the lines found to stop the search.
The lines received before it stops are all shown, maybe more."
  :type 'natnum)

(defcustom mygreper-min-input 3
  "Minimum number of the chars of the input to search."
  :type 'natnum)

(defcustom mygreper-delay 0.15
  "Seconds to wait after the last input before searching."
  :type 'number)

(defcustom mygreper-render-interval 0.1
  "Seconds to wait before showing the lines found while searching."
  :type 'number)

(defun mygreper-regexp-default (input)
  "Return the regexp of ripgrep of INPUT.
INPUT is a regexp of ripgrep, but spaces match any string."
  (string-replace " " ".*" input))

(defcustom mygreper-regexp-function #'mygreper-regexp-default
  "Function converting the input to the regexp of ripgrep."
  :type 'function)

(defface mygreper-current-line
  '((t :inherit highlight :extend t))
  "Face of the selected line shown in the minibuffer.")

(defvar mygreper-history nil
  "History of the input of `mygreper'.")

(defvar mygreper-move-hook nil
  "Hook run in the window previewing after moving to the selected line.")

;;; ripgrep

(defun mygreper--command (regexp)
  "Return the command of ripgrep searching REGEXP."
  `(,mygreper-program
    "--color=always" "--line-number" "--no-heading" "--with-filename"
    "--smart-case"
    ,@mygreper-args
    "-e" ,regexp
    ;; Not the standard input.
    "."))

;;; parse

(defun mygreper--colorize (output)
  "Return OUTPUT, an output line of ripgrep, with the colors as properties."
  ;; Each line has its colors.
  (let ((ansi-color-context nil))
    (ansi-color-apply output)))

(defconst mygreper--line-regexp "\\`\\(?:\\./\\)?\\(.+?\\):\\([0-9]+\\):"
  "Regexp of a matching line of ripgrep, of the file name and line number.")

(defun mygreper--parse (output)
  "Return (FILE LINE COLUMN) of OUTPUT, a matching line of ripgrep.
COLUMN is of the first match, colored by ripgrep.  Return nil if OUTPUT
is not of a matching line."
  (setq output (mygreper--colorize output))
  (when (string-match mygreper--line-regexp output)
    (let* ((start (match-end 0))
           ;; The colored after the line number.
           (match (text-property-not-all start (length output) 'font-lock-face nil output)))
      (list (match-string-no-properties 1 output)
            (string-to-number (match-string 2 output))
            (if match (- match start) 0)))))

;;; minibuffer

(defvar mygreper--points nil
  "Points of the buffers previewed of the current search, as an alist.
They are restored after the search.")

(defvar-local mygreper--dir nil
  "Directory searched in the current minibuffer.")

(defvar-local mygreper--window nil
  "Window previewing the selected line.")

(defvar-local mygreper--buffer nil
  "Buffer of the window before searching.")

(defvar-local mygreper--point nil
  "Point of the window before searching.")

(defvar-local mygreper--start nil
  "Start of the window before searching.")

(defvar-local mygreper--input nil
  "Input of the search, nil if none.")

(defvar-local mygreper--regexp nil
  "Regexp of ripgrep of the search.")

(defvar-local mygreper--lines []
  "Output lines of ripgrep.")

(defvar-local mygreper--index nil
  "Index of the selected line, nil if none.")

(defvar-local mygreper--first 0
  "Index of the first line shown.")

(defvar-local mygreper--overlay nil
  "Overlay showing the lines after the input.")

(defvar-local mygreper--count-overlay nil
  "Overlay showing the count of the lines before the prompt.")

(defvar-local mygreper--timer nil
  "Timer starting the search.")

(defvar-local mygreper--render-timer nil
  "Timer showing the lines received.")

(defvar-local mygreper--pending nil
  "Lines received but not shown yet, in reverse order.")

(defvar-local mygreper--pending-count 0
  "Number of the lines received but not shown yet.")

(defvar-local mygreper--process nil
  "Process of the search.")

(defvar-local mygreper--status nil
  "Status of the search, `running', `done', `limit' or nil if none.")

(defvar-local mygreper--fresh nil
  "Non-nil if no output of the process is received yet.")

(defvar-local mygreper--partial ""
  "Last incomplete line of the output of the process.")

(defun mygreper--show-selected ()
  "Scroll the lines shown to show the selected one."
  (when mygreper--index
    (setq mygreper--first
          (max (- mygreper--index (1- mygreper-height))
               (min mygreper--first mygreper--index)))))

(defun mygreper--render ()
  "Show the lines after the input."
  (let* ((lines mygreper--lines)
         (index mygreper--index)
         (first mygreper--first)
         (end (min (length lines) (+ first mygreper-height)))
         (total (number-to-string (length lines)))
         ;; For the cursor, which is not shown on a newline.
         (text " "))
    ;; Pad the index, to keep the prompt still while moving.
    (overlay-put mygreper--count-overlay 'before-string
                 (format "%s/%s%s " (string-pad (if index (number-to-string (1+ index)) "0")
                                                (length total) nil t)
                         total
                         (pcase mygreper--status
                           ('running "~")
                           ('limit "+")
                           (_ ""))))
    (cl-loop
     for i from first below end
     for line = (mygreper--colorize (aref lines i))
     do (when (eql i index)
          (font-lock-append-text-property 0 (length line) 'font-lock-face 'mygreper-current-line line))
     do (setq text (concat text "\n" line)))
    ;; Show the cursor before the lines, rather than after them.
    (put-text-property 0 1 'cursor t text)
    (move-overlay mygreper--overlay (point-max) (point-max))
    (overlay-put mygreper--overlay 'after-string text)))

(defun mygreper--find-file (file)
  "Return the buffer of FILE, or nil if it is too large to preview."
  (or (get-file-buffer file)
      (let ((size (file-attribute-size (file-attributes file))))
        ;; Not asking whether to open it.
        (unless (and size large-file-warning-threshold
                     (> size large-file-warning-threshold))
          (find-file-noselect file t)))))

(defun mygreper--goto (line column)
  "Move to COLUMN of LINE, kept in the line."
  (widen)
  (goto-char (point-min))
  (forward-line (1- line))
  (goto-char (min (+ (point) column) (pos-eol))))

(defun mygreper--preview ()
  "Show the selected line in the window."
  (let* ((selected (and mygreper--index
                         (mygreper--parse (aref mygreper--lines mygreper--index))))
         (file (and selected (expand-file-name (car selected) mygreper--dir)))
         (line (nth 1 selected))
         (column (nth 2 selected))
         (buffer mygreper--buffer)
         (point mygreper--point)
         (start mygreper--start)
         (preview (and file (mygreper--find-file file))))
    (with-selected-window mygreper--window
      (if (not preview)
          (progn
            (switch-to-buffer buffer t t)
            (set-window-point nil point)
            (set-window-start nil start t))
        (unless (assq preview mygreper--points)
          (push (cons preview (with-current-buffer preview (point)))
                mygreper--points))
        (switch-to-buffer preview t t)
        (mygreper--goto line column)
        (unless (pos-visible-in-window-p)
          (recenter)))
      (run-hooks 'mygreper-move-hook))))

(defun mygreper--refresh ()
  "Show the lines, and preview the selected one."
  (mygreper--show-selected)
  (mygreper--render)
  (mygreper--preview))

;;; process

(defun mygreper--kill ()
  "Stop the search, discarding the lines not shown yet."
  (when mygreper--timer
    (cancel-timer mygreper--timer)
    (setq mygreper--timer nil))
  (when mygreper--render-timer
    (cancel-timer mygreper--render-timer)
    (setq mygreper--render-timer nil))
  (setq mygreper--pending nil
        mygreper--pending-count 0)
  (when-let* ((process mygreper--process))
    ;; Ignored by its sentinel.
    (setq mygreper--process nil)
    (delete-process process)))

(defun mygreper--flush ()
  "Show the lines received, and refresh."
  (when mygreper--render-timer
    (cancel-timer mygreper--render-timer)
    (setq mygreper--render-timer nil))
  (let ((index mygreper--index)
        (new (vconcat (nreverse mygreper--pending))))
    (setq mygreper--pending nil
          mygreper--pending-count 0)
    (if (not mygreper--fresh)
        (setq mygreper--lines (vconcat mygreper--lines new))
      ;; Replace the lines of the last search only now, not to show
      ;; nothing while waiting.
      (setq mygreper--fresh nil
            mygreper--lines new
            mygreper--index nil
            mygreper--first 0))
    ;; Keep the lines received after the limit.
    (when (>= (length mygreper--lines) mygreper-limit)
      (setq mygreper--status 'limit)
      (mygreper--kill))
    (when (and (not mygreper--index) (length> mygreper--lines 0))
      (setq mygreper--index 0))
    (if (eq index mygreper--index)
        (mygreper--render)
      (mygreper--refresh))))

(defun mygreper--flush-minibuffer (minibuffer)
  "Show the lines received in MINIBUFFER."
  (when (buffer-live-p minibuffer)
    (with-current-buffer minibuffer
      (setq mygreper--render-timer nil)
      (mygreper--flush))))

(defun mygreper--receive (outputs)
  "Receive OUTPUTS, output lines of ripgrep, also of the errors.
The lines are shown after `mygreper-render-interval' seconds, or at once
if reaching `mygreper-limit'."
  (dolist (output outputs)
    (unless (string-empty-p output)
      (push output mygreper--pending)
      (setq mygreper--pending-count (1+ mygreper--pending-count))))
  (cond
   ((>= (+ (if mygreper--fresh 0 (length mygreper--lines)) mygreper--pending-count)
        mygreper-limit)
    (mygreper--flush))
   ((and mygreper--pending (not mygreper--render-timer))
    (setq mygreper--render-timer
          (run-with-timer mygreper-render-interval nil #'mygreper--flush-minibuffer
                          (current-buffer))))))

(defun mygreper--filter (process output)
  "Receive OUTPUT of PROCESS."
  (let ((minibuffer (process-get process 'mygreper-minibuffer)))
    (when (buffer-live-p minibuffer)
      (with-current-buffer minibuffer
        (when (eq process mygreper--process)
          (let ((outputs (split-string (concat mygreper--partial output) "\n")))
            (setq mygreper--partial (car (last outputs)))
            (mygreper--receive (butlast outputs))))))))

(defun mygreper--sentinel (process _event)
  "Finish the search of PROCESS if it exited."
  (let ((minibuffer (process-get process 'mygreper-minibuffer)))
    (when (and (buffer-live-p minibuffer)
               (memq (process-status process) '(exit signal)))
      (with-current-buffer minibuffer
        (when (eq process mygreper--process)
          (setq mygreper--process nil
                mygreper--status 'done)
          (mygreper--receive (list mygreper--partial))
          ;; At once, even if nothing found.
          (mygreper--flush))))))

(defun mygreper--start (minibuffer)
  "Start the search of MINIBUFFER."
  (when (buffer-live-p minibuffer)
    (with-current-buffer minibuffer
      (mygreper--kill)
      (let* ((default-directory mygreper--dir)
             (process (make-process
                       :name "mygreper"
                       :command (mygreper--command mygreper--regexp)
                       :connection-type 'pipe
                       :coding 'utf-8
                       :noquery t
                       :filter #'mygreper--filter
                       :sentinel #'mygreper--sentinel)))
        (process-put process 'mygreper-minibuffer minibuffer)
        (setq mygreper--process process
              mygreper--status 'running
              mygreper--fresh t
              mygreper--partial ""))
      (mygreper--render))))

(defun mygreper--update ()
  "Search the input after a delay if changed."
  (let ((input (minibuffer-contents-no-properties)))
    (unless (equal input mygreper--input)
      (setq mygreper--input input)
      (mygreper--kill)
      (setq mygreper--regexp (funcall mygreper-regexp-function input))
      (if (length< input mygreper-min-input)
          (setq mygreper--lines []
                mygreper--index nil
                mygreper--first 0
                mygreper--status nil)
        (setq mygreper--status 'running
              mygreper--timer (run-with-timer mygreper-delay nil #'mygreper--start
                                              (current-buffer))))
      (mygreper--refresh))))

(defun mygreper--post-command ()
  "Update the search, unless more input is pending."
  (unless (input-pending-p)
    (mygreper--update)))

;;; commands

(defun mygreper--select (index)
  "Select the line of INDEX, kept in the found lines."
  (when mygreper--index
    (setq mygreper--index
          (max 0 (min index (1- (length mygreper--lines)))))
    (mygreper--refresh)))

(defun mygreper-next (n)
  "Select the next N line."
  (interactive "p")
  (mygreper--select (+ (or mygreper--index 0) n)))

(defun mygreper-previous (n)
  "Select the previous N line."
  (interactive "p")
  (mygreper--select (- (or mygreper--index 0) n)))

(defun mygreper-first ()
  "Select the first line."
  (interactive)
  (mygreper--select 0))

(defun mygreper-last ()
  "Select the last line."
  (interactive)
  (mygreper--select (1- (length mygreper--lines))))

(defun mygreper-scroll-up ()
  "Select the line a page after."
  (interactive)
  (mygreper-next mygreper-height))

(defun mygreper-scroll-down ()
  "Select the line a page before."
  (interactive)
  (mygreper-previous mygreper-height))

(defun mygreper--add-history ()
  "Add the input to the history, as exiting the minibuffer normally does."
  (let ((input (minibuffer-contents-no-properties)))
    (when (and history-add-new-input (not (string-empty-p input)))
      (add-to-history minibuffer-history-variable input))))

(defun mygreper-exit ()
  "Visit the selected line."
  (interactive)
  (pcase-let ((`(,file ,line ,column)
               (or (and mygreper--index
                        (mygreper--parse (aref mygreper--lines mygreper--index)))
                   (user-error "No matching line"))))
    (mygreper--add-history)
    (throw 'mygreper--exit
           (list 'find (expand-file-name file mygreper--dir) line column))))

(defun mygreper-grep ()
  "Run the search in a `grep-mode' buffer."
  (interactive)
  (when (string-empty-p mygreper--input)
    (user-error "No input"))
  (mygreper--add-history)
  (throw 'mygreper--exit
         (list 'grep mygreper--dir mygreper--regexp)))

(defun mygreper-recenter-top-bottom (&optional arg)
  "Recenter the window of the search, as `recenter-top-bottom' with ARG."
  (interactive "P")
  (with-selected-window mygreper--window
    (recenter-top-bottom arg)))

(defvar-keymap mygreper-map
  :parent minibuffer-local-map
  "<remap> <next-line>" #'mygreper-next
  "<remap> <next-line-or-history-element>" #'mygreper-next
  "<remap> <previous-line>" #'mygreper-previous
  "<remap> <previous-line-or-history-element>" #'mygreper-previous
  "<remap> <beginning-of-buffer>" #'mygreper-first
  "<remap> <minibuffer-beginning-of-buffer>" #'mygreper-first
  "<remap> <end-of-buffer>" #'mygreper-last
  "<remap> <scroll-up-command>" #'mygreper-scroll-up
  "<remap> <scroll-down-command>" #'mygreper-scroll-down
  "<remap> <exit-minibuffer>" #'mygreper-exit
  "<remap> <recenter-top-bottom>" #'mygreper-recenter-top-bottom
  "C-x C-s" #'mygreper-grep)

(defun mygreper--grep (dir regexp)
  "Search REGEXP in DIR by ripgrep in a `grep-mode' buffer."
  (let ((default-directory dir))
    (pop-to-buffer
     (compilation-start
      (mapconcat #'shell-quote-argument
                 `(,mygreper-program
                   "--no-heading" "--line-number" "--color=always"
                   "--smart-case"
                   ,@mygreper-args
                   "-e" ,regexp
                   ".")
                 " ")
      'grep-mode))))

(defun mygreper--visit (file line column)
  "Visit COLUMN of LINE of FILE."
  (xref-push-marker-stack)
  (find-file file)
  (mygreper--goto line column))

;;;###autoload
(defun mygreper (&optional initial-input)
  "Search the files in `default-directory' by ripgrep, with INITIAL-INPUT."
  (interactive)
  (let* ((dir (file-name-as-directory (expand-file-name default-directory)))
         (buffer (current-buffer))
         (window (selected-window))
         (point (point))
         (start (window-start))
         (mygreper--points nil)
         result)
    (unwind-protect
        (setq result
              (catch 'mygreper--exit
                (minibuffer-with-setup-hook
                    (lambda ()
                      (setq mygreper--dir dir
                            mygreper--window window
                            mygreper--buffer buffer
                            mygreper--point point
                            mygreper--start start
                            mygreper--overlay (make-overlay (point-max) (point-max) nil t t)
                            mygreper--count-overlay (make-overlay (point-min) (point-min)))
                      (setq-local max-mini-window-height (1+ mygreper-height))
                      (setq-local truncate-lines t)
                      ;; Show `font-lock-face' of `ansi-color-apply', as
                      ;; `font-lock-mode', which is never on in the minibuffer.
                      (setq-local char-property-alias-alist
                                  (cons '(face font-lock-face) char-property-alias-alist))
                      (add-hook 'post-command-hook #'mygreper--post-command nil t)
                      (add-hook 'minibuffer-exit-hook #'mygreper--kill nil t)
                      (mygreper--update))
                  (read-from-minibuffer (format "Grep %s: " (abbreviate-file-name dir))
                                        initial-input mygreper-map nil 'mygreper-history))
                ;; Exited without a command of it.
                nil))
      (pcase-dolist (`(,buffer . ,point) mygreper--points)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (goto-char point))))
      (when (window-live-p window)
        (set-window-buffer window buffer)
        (set-window-point window point)
        (set-window-start window start t)))
    (pcase result
      (`(find ,file ,line ,column)
       (mygreper--visit file line column))
      (`(grep ,dir ,regexp)
       (mygreper--grep dir regexp)))))

(provide 'mygreper)
;;; mygreper.el ends here
