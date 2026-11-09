;;; myswiper.el --- Search lines in the minibuffer with preview -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: matching

;;; Commentary:

;; `myswiper' lists the lines of the current buffer matching the
;; input under the input in the minibuffer, and previews the selected
;; one in the buffer, highlighting the matches in its window.
;;
;; In the minibuffer:
;;
;; - `C-n', `C-p', `M-<', `M->', `C-v' and `M-v' select a line.
;; - `RET' jumps to the selected line, and `C-g' goes back.
;; - `C-l' recenters the window of the search.
;; - `C-x C-s' runs `occur' with the input, and selects its buffer.
;;
;; The input is a regexp, but spaces match any string.  As isearch and
;; occur, the search ignores case unless the input has an upper case
;; letter.  The lines are found by searching the buffer, and only the
;; lines shown are copied and fontified, so a large buffer is searched
;; quickly.  Updating is skipped while more input is pending.
;;
;;   (keymap-global-set "C-s" #'myswiper)
;;   (setq myswiper-height 15)
;;
;; To highlight the selected line by `hl-line-mode':
;;
;;   (add-hook 'myswiper-move-hook #'hl-line-highlight)

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defgroup myswiper nil
  "Search lines in the minibuffer with preview."
  :group 'matching)

(defcustom myswiper-height 10
  "Number of the lines shown in the minibuffer."
  :type 'natnum)

(defun myswiper-regexp-default (input)
  "Return the regexp of INPUT.
INPUT is a regexp, but spaces match any string."
  (string-replace " " ".*" input))

(defcustom myswiper-regexp-function #'myswiper-regexp-default
  "Function converting the input to the regexp matching the lines."
  :type 'function)

(defface myswiper-match
  '((t :inherit match))
  "Face of the matches in the lines shown in the minibuffer.")

(defface myswiper-current-line
  '((t :inherit highlight :extend t))
  "Face of the selected line shown in the minibuffer.")

(defface myswiper-buffer-match
  '((t :inherit lazy-highlight))
  "Face of the matches in the window of the buffer.")

(defface myswiper-buffer-current-match
  '((t :inherit isearch))
  "Face of the match of the selected line in the window of the buffer.")

(defvar myswiper-history nil
  "History of the input of `myswiper'.")

(defvar myswiper-move-hook nil
  "Hook run in the window of the buffer after moving to the selected line.")

;;; search

(defun myswiper--case-fold (regexp)
  "Return `case-fold-search' for REGEXP, as isearch and occur do."
  (if (and case-fold-search search-upper-case)
      (isearch-no-upper-case-p regexp t)
    case-fold-search))

(defun myswiper--collect (regexp)
  "Return the beginnings of the lines matching REGEXP, as a vector."
  (let ((case-fold-search (myswiper--case-fold regexp))
        lines)
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp)) (re-search-forward regexp nil t))
        (goto-char (match-beginning 0))
        (push (pos-bol) lines)
        (forward-line 1)))
    (vconcat (nreverse lines))))

(defun myswiper--nearest (lines pos)
  "Return the index of the first line of LINES not before POS.
Return the last index if none, and nil if LINES is empty."
  (unless (length= lines 0)
    (let ((lo 0) (hi (length lines)))
      (while (< lo hi)
        (let ((mid (/ (+ lo hi) 2)))
          (if (< (aref lines mid) pos)
              (setq lo (1+ mid))
            (setq hi mid))))
      (min lo (1- (length lines))))))

(defun myswiper--line (bol)
  "Return the line at BOL, keeping only its faces."
  (let* ((eol (save-excursion (goto-char bol) (pos-eol)))
         (line (buffer-substring-no-properties bol eol))
         (pos bol))
    ;; Only the parts not fontified yet, unlike `font-lock-ensure'.
    (if (bound-and-true-p jit-lock-mode)
        (jit-lock-fontify-now bol eol)
      (font-lock-ensure bol eol))
    (while (< pos eol)
      (let ((next (next-property-change pos nil eol))
            (face (or (get-text-property pos 'face)
                      (get-text-property pos 'font-lock-face))))
        (when face
          (put-text-property (- pos bol) (- next bol) 'face face line))
        (setq pos next)))
    line))

;;; minibuffer

(defvar-local myswiper--buffer nil
  "Buffer searched in the current minibuffer.")

(defvar-local myswiper--window nil
  "Window previewing the selected line.")

(defvar-local myswiper--point nil
  "Point of the buffer before searching.")

(defvar-local myswiper--input nil
  "Input of the found lines, nil if none found.")

(defvar-local myswiper--regexp nil
  "Regexp of the found lines.")

(defvar-local myswiper--error nil
  "Message of the invalid regexp of the input, nil if valid.")

(defvar-local myswiper--lines []
  "Beginnings of the found lines.")

(defvar-local myswiper--index nil
  "Index of the selected line, nil if none.")

(defvar-local myswiper--first 0
  "Index of the first line shown.")

(defvar-local myswiper--overlay nil
  "Overlay showing the lines after the input.")

(defvar-local myswiper--count-overlay nil
  "Overlay showing the count of the lines before the prompt.")

(defun myswiper--show-selected ()
  "Scroll the lines shown to show the selected one."
  (when myswiper--index
    (setq myswiper--first
          (max (- myswiper--index (1- myswiper-height))
               (min myswiper--first myswiper--index)))))

(defun myswiper--render ()
  "Show the lines after the input."
  (let* ((lines myswiper--lines)
         (index myswiper--index)
         (regexp myswiper--regexp)
         (first myswiper--first)
         (empty (string-empty-p myswiper--input))
         (width (1- (window-width)))
         (end (min (length lines) (+ first myswiper-height)))
         (total (number-to-string (length lines)))
         (text (if myswiper--error
                   (propertize (format " [%s]" myswiper--error) 'face 'error)
                 ;; For the cursor, which is not shown on a newline.
                 " ")))
    ;; Pad the index, to keep the prompt still while moving.
    (overlay-put myswiper--count-overlay 'before-string
                 (format "%s/%s " (string-pad (if index (number-to-string (1+ index)) "0")
                                              (length total) nil t)
                         total))
    (with-current-buffer myswiper--buffer
      (let ((case-fold-search (myswiper--case-fold regexp))
            (number-width (length (number-to-string (line-number-at-pos (point-max)))))
            number last)
        (cl-loop
         for i from first below end
         for bol = (aref lines i)
         ;; Count from the last line shown, rather than from the
         ;; beginning of the buffer.
         do (setq number (if last
                             (+ number (count-lines last bol))
                           (line-number-at-pos bol))
                  last bol)
         for line = (concat (propertize (string-pad (number-to-string number)
                                                    number-width nil t)
                                        'face 'line-number)
                            " "
                            (myswiper--line bol))
         do (when (and (not empty) (string-match regexp line (1+ number-width)))
              (add-face-text-property (match-beginning 0) (match-end 0)
                                      'myswiper-match nil line))
         do (setq line (truncate-string-to-width line width))
         do (when (eql i index)
              (add-face-text-property 0 (length line) 'myswiper-current-line t line))
         do (setq text (concat text "\n" line)))))
    ;; Show the cursor before the lines, rather than after them.
    (put-text-property 0 1 'cursor t text)
    (move-overlay myswiper--overlay (point-max) (point-max))
    (overlay-put myswiper--overlay 'after-string text)))

(defun myswiper--make-overlay (beg end face priority)
  "Make an overlay from BEG to END with FACE and PRIORITY in the window."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'myswiper t)
    (overlay-put overlay 'window (selected-window))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'priority priority)))

(defun myswiper--preview ()
  "Move to the selected line in the window, and highlight the matches."
  (let ((pos (and myswiper--index
                  (aref myswiper--lines myswiper--index)))
        (point myswiper--point)
        (regexp myswiper--regexp)
        (empty (string-empty-p myswiper--input)))
    (with-selected-window myswiper--window
      (remove-overlays nil nil 'myswiper t)
      (if (not pos)
          (goto-char point)
        (let ((case-fold-search (myswiper--case-fold regexp)))
          (goto-char pos)
          (when (and (not empty) (re-search-forward regexp nil t))
            (goto-char (match-beginning 0))
            (myswiper--make-overlay (match-beginning 0) (match-end 0)
                                         'myswiper-buffer-current-match 1001))
          (unless (pos-visible-in-window-p)
            (recenter))
          (unless empty
            (save-excursion
              (let ((end (window-end nil t)))
                (goto-char (window-start))
                (while (and (< (point) end) (re-search-forward regexp end t))
                  (if (< (match-beginning 0) (match-end 0))
                      (myswiper--make-overlay (match-beginning 0) (match-end 0)
                                                   'myswiper-buffer-match 1000)
                    (forward-char 1))))))))
      (run-hooks 'myswiper-move-hook))))

(defun myswiper--refresh ()
  "Show the lines, and preview the selected one."
  (myswiper--show-selected)
  (myswiper--render)
  (myswiper--preview))

(defun myswiper--update ()
  "Find the lines matching the input if changed, and refresh."
  (let ((input (minibuffer-contents-no-properties)))
    (unless (equal input myswiper--input)
      (setq myswiper--input input)
      ;; Hooks run with `inhibit-quit'.
      (condition-case err
          (let* ((inhibit-quit nil)
                 (regexp (funcall myswiper-regexp-function input))
                 (point myswiper--point)
                 (lines nil)
                 (bol nil))
            (with-current-buffer myswiper--buffer
              (setq lines (myswiper--collect regexp)
                    bol (save-excursion (goto-char point) (pos-bol))))
            (let ((index (myswiper--nearest lines bol)))
            (setq myswiper--regexp regexp
                  myswiper--lines lines
                  myswiper--index index
                  myswiper--error nil
                  ;; The selected line in the middle.
                  myswiper--first (max 0 (- (or index 0) (/ myswiper-height 2))))))
        ;; Keep the lines of the last valid input.
        (invalid-regexp
         (setq myswiper--error (cadr err)))
        (quit
         (setq myswiper--input nil
               myswiper--error "Quit")))
      (myswiper--refresh))))

(defun myswiper--post-command ()
  "Update the lines, unless more input is pending."
  (unless (input-pending-p)
    (myswiper--update)))

;;; commands

(defun myswiper--select (index)
  "Select the line of INDEX, kept in the found lines."
  (when myswiper--index
    (setq myswiper--index
          (max 0 (min index (1- (length myswiper--lines)))))
    (myswiper--refresh)))

(defun myswiper-next (n)
  "Select the next N line."
  (interactive "p")
  (myswiper--select (+ (or myswiper--index 0) n)))

(defun myswiper-previous (n)
  "Select the previous N line."
  (interactive "p")
  (myswiper--select (- (or myswiper--index 0) n)))

(defun myswiper-first ()
  "Select the first line."
  (interactive)
  (myswiper--select 0))

(defun myswiper-last ()
  "Select the last line."
  (interactive)
  (myswiper--select (1- (length myswiper--lines))))

(defun myswiper-scroll-up ()
  "Select the line a page after."
  (interactive)
  (myswiper-next myswiper-height))

(defun myswiper-scroll-down ()
  "Select the line a page before."
  (interactive)
  (myswiper-previous myswiper-height))

(defun myswiper--add-history ()
  "Add the input to the history, as exiting the minibuffer normally does."
  (let ((input (minibuffer-contents-no-properties)))
    (when (and history-add-new-input (not (string-empty-p input)))
      (add-to-history minibuffer-history-variable input))))

(defun myswiper-exit ()
  "Jump to the selected line."
  (interactive)
  (unless myswiper--index
    (user-error "No line"))
  (myswiper--add-history)
  (throw 'myswiper--exit
         (list 'goto
               (window-point myswiper--window)
               (window-start myswiper--window))))

(defun myswiper-occur ()
  "Run `occur' with the input."
  (interactive)
  (when (or myswiper--error (string-empty-p myswiper--input))
    (user-error "No valid input"))
  (myswiper--add-history)
  (throw 'myswiper--exit (list 'occur myswiper--regexp)))

(defun myswiper-recenter-top-bottom (&optional arg)
  "Recenter the window of the search, as `recenter-top-bottom' with ARG."
  (interactive "P")
  (with-selected-window myswiper--window
    (recenter-top-bottom arg))
  ;; Highlight the matches newly shown.
  (myswiper--preview))

(defvar-keymap myswiper-map
  :parent minibuffer-local-map
  "<remap> <next-line>" #'myswiper-next
  "<remap> <next-line-or-history-element>" #'myswiper-next
  "<remap> <previous-line>" #'myswiper-previous
  "<remap> <previous-line-or-history-element>" #'myswiper-previous
  "<remap> <beginning-of-buffer>" #'myswiper-first
  "<remap> <minibuffer-beginning-of-buffer>" #'myswiper-first
  "<remap> <end-of-buffer>" #'myswiper-last
  "<remap> <scroll-up-command>" #'myswiper-scroll-up
  "<remap> <scroll-down-command>" #'myswiper-scroll-down
  "<remap> <exit-minibuffer>" #'myswiper-exit
  "<remap> <recenter-top-bottom>" #'myswiper-recenter-top-bottom
  "C-x C-s" #'myswiper-occur)

;;;###autoload
(defun myswiper (&optional initial-input)
  "Search the lines of the current buffer, starting with INITIAL-INPUT."
  (interactive)
  (let ((buffer (current-buffer))
        (window (selected-window))
        (point (point))
        (start (window-start))
        result)
    (unwind-protect
        (setq result
              (catch 'myswiper--exit
                (minibuffer-with-setup-hook
                    (lambda ()
                      (setq myswiper--buffer buffer
                            myswiper--window window
                            myswiper--point point
                            myswiper--overlay (make-overlay (point-max) (point-max) nil t t)
                            myswiper--count-overlay (make-overlay (point-min) (point-min)))
                      (setq-local max-mini-window-height (1+ myswiper-height))
                      (add-hook 'post-command-hook #'myswiper--post-command nil t)
                      (myswiper--update))
                  (read-from-minibuffer "Swiper: " initial-input myswiper-map
                                        nil 'myswiper-history))
                ;; Exited without a command of it.
                nil))
      (with-current-buffer buffer
        (remove-overlays nil nil 'myswiper t))
      (when (window-live-p window)
        (pcase result
          (`(goto ,pos ,new-start)
           (with-selected-window window
             (unless (= pos point)
               (push-mark point t))
             (goto-char pos)
             (set-window-start window new-start t)))
          (_
           (set-window-point window point)
           (set-window-start window start t)))))
    (pcase result
      (`(occur ,regexp)
       (occur regexp)
       (when-let* ((window (get-buffer-window "*Occur*")))
         (select-window window))))))

(provide 'myswiper)
;;; myswiper.el ends here
