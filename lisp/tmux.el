;;; tmux.el --- Fetch and push text to tmux panes -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: terminals

;;; Commentary:

;; `tmux-pane-list', or `tmux', lists the panes of the tmux server.  It
;; is for the terminal applications reading long input, such as an
;; agent: push the text edited in Emacs to a pane, and fetch the pane to
;; read it in Emacs.
;;
;; tmux runs as `tmux-program', which may be a script running tmux on
;; another host by ssh, such as named "tmux" in `exec-path'.
;;
;; The list of panes, the fetch buffer and the push buffer of a pane
;; each run at most a process of tmux asynchronously, shown in the
;; mode line, and killed by `C-c C-k'.  Killing the buffer or Emacs
;; asks to kill the process first.  The list and the fetch buffer are
;; updated by `C-c C-f', not by `revert-buffer', and when created by
;; the commands showing them.
;;
;; In the list of panes, `C-c C-f' lists the panes, and `RET' shows the
;; fetch buffer of the pane.
;;
;; In the fetch buffer and the push buffer of a pane, `C-c C-z'
;; switches to the other one, created if not yet.
;;
;; In the fetch buffer, `C-c C-f' fetches the pane, with colors and at
;; most `tmux-pane-fetch-history' lines of the history above the screen, or
;; as many as read with `C-u', and replaces the buffer by it, also by
;; the part fetched if killed.  It is ignored while fetching.
;;
;; In the push buffer, `C-c C-c' pastes the text into the pane without
;; the trailing newlines, and with `C-u' also presses Enter, which
;; `C-c C-m' presses alone.  The buffer is erased to edit the next
;; text, which is ignored while pushing.  The text is pasted by
;; bracketed paste if the application asks for it, so the newlines in
;; the text don't submit it.  The text pushed is logged in the buffer
;; *tmux-log*, which is shown if failed or killed.
;;
;; The push buffer is in `text-mode', to change it:
;;
;;   (setq tmux-pane-push-major-mode #'markdown-mode)

;;; Code:

(require 'ansi-color)
(require 'tabulated-list)

(defgroup tmux nil
  "Fetch and push text to tmux panes."
  :group 'terminals)

(defcustom tmux-program "tmux"
  "The tmux program."
  :type 'string)

(defcustom tmux-pane-fetch-history 1000
  "Lines of the history fetched above the screen of a pane, or t for all."
  :type '(choice natnum (const :tag "All" t)))

(defcustom tmux-pane-push-major-mode #'text-mode
  "Major mode of the push buffer."
  :type 'function)

(defvar tmux-log-buffer-name "*tmux-log*"
  "Name of the buffer logging the text pushed.")

(defvar-local tmux--pane nil
  "The id of the pane of the buffer.")
(put 'tmux--pane 'permanent-local t)

;;; tmux

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

(defun tmux--failed-p (process output)
  "Return non-nil if PROCESS failed, after showing its OUTPUT."
  (unless (or (tmux--killed-p process) (eq (process-exit-status process) 0))
    (message "tmux: %s"
             (if (string-blank-p output)
                 (format "exited with %d" (process-exit-status process))
               (string-trim output)))))

(defun tmux--update (buffer function)
  "Call FUNCTION to update BUFFER, by the point of its window if shown."
  (if-let* ((window (get-buffer-window buffer)))
      (with-selected-window window
        (funcall function))
    (with-current-buffer buffer
      (funcall function))))

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

(defvar-keymap tmux-pane-list-mode-map
  "RET" #'tmux-pane-fetch-switch-buffer
  "C-c C-f" #'tmux-pane-list-fetch
  "C-c C-k" #'tmux-kill)

(define-derived-mode tmux-pane-list-mode tabulated-list-mode "Tmux-Pane-List"
  "Major mode of the list of the tmux panes."
  (setq tabulated-list-format
        [("Pane" 12 t) ("Command" 12 t) ("Size" 9 nil) ("Title" 0 nil)])
  (tabulated-list-init-header))

(defun tmux-pane-list-fetch--done (process output)
  "List the panes in OUTPUT of PROCESS."
  (let ((buffer (process-buffer process)))
    (when (and (buffer-live-p buffer)
               (not (tmux--killed-p process))
               (not (tmux--failed-p process output)))
      (tmux--update buffer
                    (lambda ()
                      (setq tabulated-list-entries (tmux--parse-panes output))
                      (tabulated-list-print t))))))

(defun tmux-pane-list-fetch ()
  "List the panes again, unless listing."
  (interactive)
  (if (tmux--busy-p)
      (message "Listing...")
    (tmux--start (list "list-panes" "-a" "-F" tmux--panes-format)
                 #'tmux-pane-list-fetch--done)))

;;;###autoload
(defun tmux-pane-list ()
  "List the tmux panes."
  (interactive)
  (pop-to-buffer-same-window (get-buffer-create "*tmux-pane-list*"))
  (unless (derived-mode-p 'tmux-pane-list-mode)
    (tmux-pane-list-mode)
    (tmux-pane-list-fetch)))

;;;###autoload
(defalias 'tmux #'tmux-pane-list)

(defun tmux--current-pane ()
  "Return the pane of the current buffer."
  (or (if (derived-mode-p 'tmux-pane-list-mode)
          (tabulated-list-get-id)
        tmux--pane)
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

(defvar tmux-pane-push-mode)

(defun tmux-pane-switch ()
  "Switch to the other buffer of the pane, the push or fetch one."
  (interactive)
  (cond ((derived-mode-p 'tmux-pane-fetch-mode)
         (tmux-pane-push-switch-buffer tmux--pane))
        (tmux-pane-push-mode
         (tmux-pane-fetch-switch-buffer tmux--pane))
        (t (user-error "Not a fetch or push buffer"))))

;;; fetch

(defvar-keymap tmux-pane-fetch-mode-map
  "C-c C-f" #'tmux-pane-fetch
  "C-c C-k" #'tmux-kill
  "C-c C-z" #'tmux-pane-switch)

(define-derived-mode tmux-pane-fetch-mode special-mode "Tmux-Pane-Fetch"
  "Major mode of the fetch buffer of a tmux pane."
  (setq truncate-lines t))

(defun tmux-pane-fetch-switch-buffer (&optional pane)
  "Show the fetch buffer of PANE, by default the current one, fetched if new."
  (interactive)
  (let ((pane (or pane (tmux--current-pane))))
    (pop-to-buffer (get-buffer-create (format "*tmux-pane-fetch %s*" pane)))
    (unless (derived-mode-p 'tmux-pane-fetch-mode)
      (tmux-pane-fetch-mode)
      (setq tmux--pane pane)
      (tmux-pane-fetch))))

(defun tmux-pane-fetch--render (output)
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

(defun tmux-pane-fetch--done (process output)
  "Replace the buffer of PROCESS by its OUTPUT."
  (let ((buffer (process-buffer process)))
    (cond ((not (buffer-live-p buffer)))
          ((tmux--failed-p process output))
          ((string-empty-p output)
           (message "Nothing fetched"))
          (t
           (tmux--update buffer
                         (lambda () (tmux-pane-fetch--render output)))))))

(defun tmux-pane-fetch (&optional history)
  "Fetch the pane into the current buffer, unless fetching.
Fetch HISTORY lines of the history, by default `tmux-pane-fetch-history',
interactively read with a prefix argument."
  (interactive (list (and current-prefix-arg (read-number "History lines: "))))
  (if (tmux--busy-p)
      (message "Fetching %s..." tmux--pane)
    (tmux--start (tmux--capture-args tmux--pane
                                     (or history tmux-pane-fetch-history))
                 #'tmux-pane-fetch--done)))

;;; push

(defvar-keymap tmux-pane-push-mode-map
  "C-c C-c" #'tmux-pane-push
  "C-c C-m" #'tmux-pane-push-enter
  "C-c C-k" #'tmux-kill
  "C-c C-z" #'tmux-pane-switch)

(define-minor-mode tmux-pane-push-mode
  "Minor mode of the push buffer of a tmux pane.

\\{tmux-pane-push-mode-map}"
  :lighter " Tmux-Pane-Push")
(put 'tmux-pane-push-mode 'permanent-local t)

(defun tmux-pane-push-switch-buffer (&optional pane)
  "Show the push buffer of PANE, by default the current one."
  (interactive)
  (let ((pane (or pane (tmux--current-pane))))
    (pop-to-buffer (get-buffer-create (format "*tmux-pane-push %s*" pane)))
    (unless tmux-pane-push-mode
      (setq tmux--pane pane)
      (funcall tmux-pane-push-major-mode)
      (tmux-pane-push-mode))))

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

(defun tmux-pane-push--done (marker)
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

(defun tmux-pane-push (&optional enter)
  "Paste the text into the pane, and with prefix ENTER press Enter."
  (interactive "P")
  (let ((text (string-trim-right (buffer-string) "[ \t\n]+")))
    (cond ((tmux--busy-p)
           (message "Pushing to %s..." tmux--pane))
          ((string-empty-p text)
           (user-error "No text"))
          (t
           (tmux--start (tmux--paste-args tmux--pane enter)
                        (tmux-pane-push--done (tmux--log tmux--pane text))
                        text)
           (erase-buffer)))))

(defun tmux-pane-push-enter ()
  "Press Enter in the pane."
  (interactive)
  (if (tmux--busy-p)
      (message "Pushing to %s..." tmux--pane)
    (tmux--start (list "send-keys" "-t" tmux--pane "Enter") #'tmux--failed-p)))

(provide 'tmux)
;;; tmux.el ends here
