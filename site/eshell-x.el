;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'cl-lib)
(require 'eshell)

(defvar eshell-buffer-name)
(declare-function eshell-reset "esh-mode")
(declare-function eshell/cd "em-dirs")

;;;###autoload
(defun eshell-dwim (&optional arg)
  (interactive "P")
  (let* ((window-buffer-list (mapcar #'window-buffer (window-list)))
         (buffer (cl-find-if
                  (lambda (buffer)
                    (and (eq (with-current-buffer buffer major-mode) 'eshell-mode)
                         (string-prefix-p eshell-buffer-name (buffer-name buffer))
                         (not (get-buffer-process buffer))
                         (not (member buffer window-buffer-list))))
                  (buffer-list))))
    (if buffer
        (let ((dir default-directory))
          (with-current-buffer buffer
            (eshell/cd dir)
            (eshell-reset)))
      (setq buffer (generate-new-buffer eshell-buffer-name))
      (with-current-buffer buffer
        (eshell-mode)))
    (cond ((> (prefix-numeric-value arg) 4)
           (switch-to-buffer buffer))
          (arg
           (switch-to-buffer-other-window buffer))
          (t
           (pop-to-buffer buffer '(display-buffer-at-bottom))))))

(provide 'eshell-dwim)
