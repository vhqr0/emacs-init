;;; tmux.el --- Fetch and push text to tmux panes -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: terminals

;;; Commentary:

;; `tmux' lists the panes of the tmux server.  It is for the terminal
;; applications reading long input, such as an agent: push the text
;; edited in Emacs to a pane, and fetch the pane to read it in Emacs.
;;
;; tmux runs as `tmux-program', which may be a script running tmux on
;; another host by ssh, such as named "tmux" in `exec-path'.
;;
;; In the list of panes, `RET' shows the fetch buffer of the pane, and
;; `g' lists the panes again.
;;
;; A pane has a fetch buffer and a push buffer, each running at most a
;; process of tmux asynchronously, shown in the mode line.  In both:
;;
;; - `C-c C-z' switches to the other buffer of the pane, created if
;;   not yet.
;; - `C-c C-k' kills the process.  Killing the buffer or Emacs asks to
;;   kill the process first.
;;
;; In the fetch buffer, `C-c C-f' fetches the pane, with colors and at
;; most `tmux-fetch-history' lines of the history above the screen, or
;; as many as read with `C-u', and replaces the buffer by it, also by
;; the part fetched if killed.  It is ignored while fetching.
;;
;; In the push buffer, `C-c C-c' pastes the text into the pane without
;; the trailing newlines, and with `C-u' also presses Enter.  The
;; buffer is erased to edit the next text, which is ignored while
;; pushing.  The text is pasted by bracketed paste if the application
;; asks for it, so the newlines in the text don't submit it.  The text
;; pushed is logged in the buffer *tmux-log*, which is shown if failed
;; or killed.
;;
;; The push buffer is in `text-mode', to change it:
;;
;;   (setq tmux-push-major-mode #'markdown-mode)

;;; Code:

(require 'ansi-color)
(require 'tabulated-list)

(defgroup tmux nil
  "Fetch and push text to tmux panes."
  :group 'terminals)

(defcustom tmux-program "tmux"
  "The tmux program."
  :type 'string)

(defcustom tmux-fetch-history 1000
  "Lines of the history fetched above the screen of a pane, or t for all."
  :type '(choice natnum (const :tag "All" t)))

(defcustom tmux-push-major-mode #'text-mode
  "Major mode of the push buffer."
  :type 'function)

(defvar tmux-log-buffer-name "*tmux-log*"
  "Name of the buffer logging the text pushed.")

(defvar-local tmux--pane nil
  "The id of the pane of the buffer.")
(put 'tmux--pane 'permanent-local t)

;;; tmux

(defun tmux--call (&rest args)
  "Run tmux with ARGS, and return its output."
  (let ((default-directory (expand-file-name "~/"))
        (coding-system-for-read 'utf-8-unix))
    (with-temp-buffer
      (unless (eq (apply #'call-process tmux-program nil t nil args) 0)
        (user-error "tmux: %s" (string-trim (buffer-string))))
      (buffer-string))))

(defun tmux--start (args callback &optional input)
  "Start tmux with ARGS and INPUT as stdin as the process of the buffer.
Call CALLBACK with the process and its output when it exits, also
after the buffer is killed."
  (setq mode-line-process ":run")
  (force-mode-line-update)
  (let* ((default-directory (expand-file-name "~/"))
         (process (make-process
                   :name "tmux"
                   :buffer (current-buffer)
                   :command (cons tmux-program args)
                   :connection-type 'pipe
                   :coding 'utf-8-unix
                   :filter #'tmux--filter
                   :sentinel (lambda (process _event)
                               (unless (process-live-p process)
                                 (tmux--exited process)
                                 (funcall callback process
                                          (or (process-get process :output) "")))))))
    (when input
      (process-send-string process input))
    ;; Even without INPUT, not to wait for it, such as by a script.
    (process-send-eof process)
    process))

(defun tmux--filter (process string)
  "Collect STRING of the output of PROCESS."
  (process-put process :output (concat (process-get process :output) string)))

(defun tmux--exited (process)
  "Show in the mode line that PROCESS exited."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (setq mode-line-process nil)
      (force-mode-line-update))))

(defun tmux--killed-p (process)
  "Return non-nil if PROCESS is killed, not exited."
  (or (process-get process :killed) (eq (process-status process) 'signal)))

(defun tmux--busy-p ()
  "Return non-nil if the current buffer has a process running."
  (process-live-p (get-buffer-process (current-buffer))))

(defconst tmux--panes-format
  (string-join '("#{pane_id}" "#{session_name}:#{window_index}.#{pane_index}"
                 "#{pane_current_command}" "#{pane_width}x#{pane_height}"
                 "#{pane_title}")
               "\t")
  "Format of a pane listed, parsed by `tmux--parse-panes'.")

(defun tmux--parse-panes (output)
  "Return the entries of `tabulated-list-entries' of the panes in OUTPUT.
A pane in more than one session, as grouped, is the first one only."
  (let ((entries nil))
    (dolist (line (split-string output "\n" t))
      (pcase-let ((`(,id ,location ,command ,size . ,title) (split-string line "\t")))
        (unless (assoc id entries)
          (push (list id (vector location command size (string-join title "\t")))
                entries))))
    (nreverse entries)))

(defun tmux--capture-args (pane history)
  "Return the args of tmux to capture PANE with HISTORY lines, or t for all."
  (list "capture-pane" "-p" "-e" "-t" pane "-S"
        (if (eq history t) "-" (format "-%d" history))))

(defun tmux--paste-args (pane enter)
  "Return the args of tmux to paste stdin into PANE, and press ENTER."
  `("load-buffer" "-b" "tmux-el" "-"
    ";" "paste-buffer" "-p" "-d" "-b" "tmux-el" "-t" ,pane
    ,@(and enter (list ";" "send-keys" "-t" pane "Enter"))))

;;; list

(defvar-keymap tmux-list-mode-map
  "RET" #'tmux-fetch)

(define-derived-mode tmux-list-mode tabulated-list-mode "Tmux"
  "Major mode of the list of the tmux panes."
  (setq tabulated-list-format
        [("Pane" 12 t) ("Command" 12 t) ("Size" 9 nil) ("Title" 0 nil)])
  (add-hook 'tabulated-list-revert-hook #'tmux-list--refresh nil t)
  (tabulated-list-init-header))

(defun tmux-list--refresh ()
  "Set `tabulated-list-entries' to the panes."
  (setq tabulated-list-entries
        (tmux--parse-panes (tmux--call "list-panes" "-a" "-F" tmux--panes-format))))

;;;###autoload
(defun tmux ()
  "List the tmux panes."
  (interactive)
  (pop-to-buffer-same-window (get-buffer-create "*tmux*"))
  (unless (derived-mode-p 'tmux-list-mode)
    (tmux-list-mode))
  (revert-buffer))

(defun tmux--current-pane ()
  "Return the pane of the current buffer."
  (or (if (derived-mode-p 'tmux-list-mode) (tabulated-list-get-id) tmux--pane)
      (user-error "No pane")))

;;; process

(defun tmux-kill ()
  "Kill the process of the current buffer."
  (interactive)
  (let ((process (get-buffer-process (current-buffer))))
    (unless (process-live-p process)
      (user-error "No process"))
    (process-put process :killed t)
    (kill-process process)))

(defvar tmux-push-mode)

(defun tmux-switch ()
  "Switch to the other buffer of the pane, the push or fetch one."
  (interactive)
  (cond ((derived-mode-p 'tmux-fetch-mode) (tmux-push tmux--pane))
        (tmux-push-mode (tmux-fetch tmux--pane))
        (t (user-error "Not a fetch or push buffer"))))

;;; fetch

(defvar-keymap tmux-fetch-mode-map
  "C-c C-f" #'tmux-fetch-update
  "C-c C-k" #'tmux-kill
  "C-c C-z" #'tmux-switch)

(define-derived-mode tmux-fetch-mode special-mode "Tmux-Fetch"
  "Major mode of the fetch buffer of a tmux pane."
  (setq truncate-lines t))

(defun tmux-fetch (&optional pane)
  "Show the fetch buffer of PANE, by default the current one."
  (interactive)
  (let ((pane (or pane (tmux--current-pane))))
    (pop-to-buffer (get-buffer-create (format "*tmux-fetch %s*" pane)))
    (unless (derived-mode-p 'tmux-fetch-mode)
      (tmux-fetch-mode)
      (setq tmux--pane pane))))

(defun tmux-fetch--render (output)
  "Replace the current buffer by OUTPUT fetched from the pane.
Keep the line of point, or the end if point is on the last line."
  (let ((line (line-number-at-pos))
        (column (current-column))
        (end (>= (line-number-at-pos) (line-number-at-pos (point-max))))
        (inhibit-read-only t))
    (erase-buffer)
    ;; Without the empty lines below the screen.
    (insert (string-trim-right (ansi-color-apply output)) "\n")
    (if end
        (goto-char (point-max))
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column column))))

(defun tmux-fetch--done (process output)
  "Replace the buffer of PROCESS by its OUTPUT."
  (let ((buffer (process-buffer process)))
    (cond ((not (buffer-live-p buffer)))
          ((and (not (tmux--killed-p process))
                (not (eq (process-exit-status process) 0)))
           (message "tmux: %s" (string-trim output)))
          ((string-empty-p output)
           (message "Nothing fetched"))
          ;; Point of the window, not of the buffer.
          ((get-buffer-window buffer)
           (with-selected-window (get-buffer-window buffer)
             (tmux-fetch--render output)))
          (t
           (with-current-buffer buffer
             (tmux-fetch--render output))))))

(defun tmux-fetch-update (&optional history)
  "Fetch the pane into the current buffer, unless fetching.
Fetch HISTORY lines of the history, by default `tmux-fetch-history',
interactively read with a prefix argument."
  (interactive (list (and current-prefix-arg (read-number "History lines: "))))
  (if (tmux--busy-p)
      (message "Fetching %s..." tmux--pane)
    (tmux--start (tmux--capture-args tmux--pane (or history tmux-fetch-history))
                 #'tmux-fetch--done)))

;;; push

(defvar-keymap tmux-push-mode-map
  "C-c C-c" #'tmux-push-send
  "C-c C-k" #'tmux-kill
  "C-c C-z" #'tmux-switch)

(define-minor-mode tmux-push-mode
  "Minor mode of the push buffer of a tmux pane.

\\{tmux-push-mode-map}"
  :lighter " Tmux-Push")
(put 'tmux-push-mode 'permanent-local t)

(defun tmux-push (&optional pane)
  "Show the push buffer of PANE, by default the current one."
  (interactive)
  (let ((pane (or pane (tmux--current-pane))))
    (pop-to-buffer (get-buffer-create (format "*tmux-push %s*" pane)))
    (unless tmux-push-mode
      (setq tmux--pane pane)
      (funcall tmux-push-major-mode)
      (tmux-push-mode))))

(defun tmux--log (pane text)
  "Log pushing TEXT to PANE, and return the marker of its status."
  (with-current-buffer (get-buffer-create tmux-log-buffer-name)
    (unless (derived-mode-p 'special-mode)
      (special-mode))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (insert (format-time-string "* %F %T ") pane " ")
        (prog1 (point-marker)
          (insert "pushing\n" text "\n\n"))))))

(defun tmux--log-status (marker status)
  "Replace the status at MARKER of the log by STATUS."
  (when (buffer-live-p (marker-buffer marker))
    (with-current-buffer (marker-buffer marker)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char marker)
          (delete-region (point) (line-end-position))
          (insert status))))))

(defun tmux-push--done (marker)
  "Return the callback of the process pushing the text logged at MARKER."
  (lambda (process output)
    (if (and (not (tmux--killed-p process))
             (eq (process-exit-status process) 0))
        (tmux--log-status marker "pushed")
      (tmux--log-status marker (if (tmux--killed-p process)
                                   "killed, maybe pushed"
                                 (format "failed: %s" (string-trim output))))
      (when (buffer-live-p (marker-buffer marker))
        (set-window-point (display-buffer (marker-buffer marker)) marker)))))

(defun tmux-push-send (&optional enter)
  "Paste the text into the pane, and with prefix ENTER press Enter."
  (interactive "P")
  (let ((text (string-trim-right (buffer-string) "[ \t\n]+")))
    (cond ((tmux--busy-p)
           (message "Pushing to %s..." tmux--pane))
          ((string-empty-p text)
           (user-error "No text"))
          (t
           (tmux--start (tmux--paste-args tmux--pane enter)
                        (tmux-push--done (tmux--log tmux--pane text))
                        text)
           (erase-buffer)))))

(provide 'tmux)
;;; tmux.el ends here
