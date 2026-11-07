;;; pair-x.el --- Edit the pairs of lists -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: lisp, convenience

;;; Commentary:

;; Paredit like commands on the list enclosing point, by its syntax:
;;
;; - `pair-x-splice-sexp' deletes its delimiters.
;; - `pair-x-forward-slurp-sexp' and `pair-x-backward-slurp-sexp' move
;;   its closing or opening delimiter over the next or previous sexp,
;;   to take it in.
;; - `pair-x-forward-barf-sexp' and `pair-x-backward-barf-sexp' move
;;   its closing or opening delimiter over its last or first sexp, to
;;   put it out.
;;
;; Strings are not lists: in a string, the list enclosing the string is
;; edited.  The lines changed are reindented.
;;
;;   (keymap-global-set "C-<right>" #'pair-x-forward-slurp-sexp)
;;   (keymap-global-set "C-<left>" #'pair-x-forward-barf-sexp)
;;   (keymap-global-set "C-M-<left>" #'pair-x-backward-slurp-sexp)
;;   (keymap-global-set "C-M-<right>" #'pair-x-backward-barf-sexp)

;;; Code:

(defun pair-x--list ()
  "Return (BEG . END) of the list enclosing point.
END is after its closing delimiter."
  (let ((beg (or (nth 1 (syntax-ppss))
                 (user-error "Not in a list"))))
    (cons beg (scan-sexps beg 1))))

(defun pair-x--scan (from count)
  "Return the position COUNT sexps from FROM, or nil if none."
  (condition-case nil
      (scan-sexps from count)
    (scan-error nil)))

;;;###autoload
(defun pair-x-splice-sexp ()
  "Delete the delimiters of the list enclosing point."
  (interactive)
  (pcase-let ((`(,beg . ,end) (pair-x--list)))
    (save-excursion
      (goto-char (1- end))
      (delete-char 1)
      (goto-char beg)
      (delete-char 1)
      (indent-region beg (- end 2)))))

;;;###autoload
(defun pair-x-forward-slurp-sexp ()
  "Move the closing delimiter of the list enclosing point over the next sexp."
  (interactive)
  (pcase-let* ((`(,beg . ,end) (pair-x--list))
               (to (or (pair-x--scan end 1)
                       (user-error "Nothing to slurp"))))
    (save-excursion
      (goto-char to)
      (insert (char-before end))
      (goto-char end)
      (delete-char -1)
      (indent-region beg to))))

;;;###autoload
(defun pair-x-forward-barf-sexp ()
  "Move the closing delimiter of the list enclosing point before its last sexp."
  (interactive)
  (pcase-let* ((`(,beg . ,end) (pair-x--list))
               (last (or (pair-x--scan (1- end) -1)
                         (user-error "Nothing to barf")))
               (close (char-before end)))
    (save-excursion
      (goto-char (1- end))
      (delete-char 1)
      ;; After the previous sexp, or the opening delimiter.
      (goto-char last)
      (forward-comment (- (point)))
      (insert close)
      (indent-region beg end))))

;;;###autoload
(defun pair-x-backward-slurp-sexp ()
  "Move the opening delimiter of the list enclosing point over the previous sexp."
  (interactive)
  (pcase-let* ((`(,beg . ,end) (pair-x--list))
               (to (or (pair-x--scan beg -1)
                       (user-error "Nothing to slurp")))
               (open (char-after beg)))
    (save-excursion
      (goto-char beg)
      (delete-char 1)
      (goto-char to)
      (insert open)
      (indent-region to end))))

;;;###autoload
(defun pair-x-backward-barf-sexp ()
  "Move the opening delimiter of the list enclosing point after its first sexp."
  (interactive)
  (pcase-let* ((`(,beg . ,end) (pair-x--list))
               (first (or (pair-x--scan (1+ beg) 1)
                          (user-error "Nothing to barf")))
               (open (char-after beg)))
    (save-excursion
      ;; Before the next sexp, or the closing delimiter.
      (goto-char first)
      (forward-comment (buffer-size))
      (insert open)
      (goto-char beg)
      (delete-char 1)
      (indent-region beg end))))

(provide 'pair-x)
;;; pair-x.el ends here
