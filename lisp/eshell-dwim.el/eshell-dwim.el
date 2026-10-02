;;; eshell-dwim.el --- Open eshell smartly -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: convenience

;;; Commentary:

;; Open an idle eshell buffer in `default-directory', reusing one when
;; possible.  Call `eshell-dwim'.

;;; Code:

(require 'eshell)
(require 'esh-mode)
(require 'em-dirs)

(defun eshell-dwim-find-buffer ()
  "Find eshell dwim buffer."
  (seq-find
   (lambda (buffer)
     (and (eq (buffer-local-value 'major-mode buffer) 'eshell-mode)
          (string-prefix-p eshell-buffer-name (buffer-name buffer))
          (not (get-buffer-process buffer))
          (not (get-buffer-window buffer))))
   (buffer-list)))

(defun eshell-dwim-get-buffer-create ()
  "Get eshell dwim buffer, create if not exist."
  (if-let* ((buffer (eshell-dwim-find-buffer)))
      (let ((dir default-directory))
        (with-current-buffer buffer
          (eshell/cd dir)
          (eshell-reset)
          (current-buffer)))
    (with-current-buffer (generate-new-buffer eshell-buffer-name)
      (eshell-mode)
      (current-buffer))))

(defun eshell-dwim-switch-to-buffer-split-window (buffer)
  "Switch to BUFFER split at this window."
  (let ((parent (window-parent (selected-window))))
    (cond ((window-left-child parent)
           (select-window (split-window-vertically))
           (switch-to-buffer buffer))
          ((window-top-child parent)
           (select-window (split-window-horizontally))
           (switch-to-buffer buffer))
          (t
           (switch-to-buffer-other-window buffer)))))

;;;###autoload
(defun eshell-dwim (&optional arg)
  "Do open eshell smartly.
Without universal ARG, open in split window.
With universal ARG, open in other window.
With two universal ARG, open in this window."
  (interactive "P")
  (let ((buffer (eshell-dwim-get-buffer-create)))
    (cond ((> (prefix-numeric-value arg) 4)
           (switch-to-buffer buffer))
          (arg
           (switch-to-buffer-other-window buffer))
          (t
           (eshell-dwim-switch-to-buffer-split-window buffer)))))

(provide 'eshell-dwim)
;;; eshell-dwim.el ends here
