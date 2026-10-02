;;; timestamp-at-point.el --- Convert the timestamp at point -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: convenience

;;; Commentary:

;; Convert the number at point to a readable time, as a duration or a
;; posix timestamp.  Call `timestamp-at-point'.

;;; Code:

(defun timestamp-at-point-convert (ts)
  "Convert TS to time string dwim.
Support:
- seconds and milliseconds duration in one day.
- seconds and milliseconds posix timestamp from 2001 to 2286."
  (cond
   ((<= 90 ts 86400)
    (format "%02d:%02d:%02d"
            (/ ts 3600)
            (mod (/ ts 60) 60)
            (mod ts 60)))
   ((<= 90000 ts 86400000)
    (format "%02d:%02d:%02d:%03d"
            (/ ts 3600000)
            (mod (/ ts 60000) 60)
            (mod (/ ts 1000) 60)
            (mod ts 1000)))
   ((<= 1000000000 ts 9999999999)
    (format-time-string "%Z %Y-%m-%d %H:%M:%S" ts))
   ((<= 1000000000000 ts 9999999999999)
    (let ((ts (list 0 (/ ts 1000) (* 1000 (% ts 1000)) 0)))
      (format-time-string "%Z %Y-%m-%d %H:%M:%S:%3N" ts)))))

;;;###autoload
(defun timestamp-at-point (&optional arg)
  "Echo the converted timestamp at point.
With one universal ARG, also copy it to the kill ring.
With two universal ARG, also replace the timestamp with it."
  (interactive "P")
  (if-let* ((ts (thing-at-point 'number)))
      (if-let* ((s (timestamp-at-point-convert ts)))
          (progn
            (when arg
              (kill-new s))
            (when (> (prefix-numeric-value arg) 4)
              (let ((bounds (bounds-of-thing-at-point 'number)))
                (replace-region-contents (car bounds) (cdr bounds) s)))
            (message "%s" s))
        (user-error "Not a timestamp"))
    (user-error "No number at point")))

(provide 'timestamp-at-point)
;;; timestamp-at-point.el ends here
