;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'evil)

(defun prog-x-format-default-function ()
  (delete-trailing-whitespace (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(defun prog-x-format-default-get-command-function ()
  #'prog-x-format-default-function)

(defvar-local prog-x-format-get-command-function #'prog-x-format-default-get-command-function)

(defun prog-x-format-region (&optional beg end)
  (interactive "*r")
  (save-restriction
    (narrow-to-region beg end)
    (let ((line-number (line-number-at-pos))
          (command (funcall prog-x-format-get-command-function)))
      (cond ((functionp command)
             (funcall command))
            ((stringp command)
             (shell-command-on-region (point-min) (point-max) command nil t))
            (t
             (error (format "prog-x-format-region: invalid type: %s"
                            (symbol-name (type-of command))))))
      (goto-char (point-min))
      (forward-line (1- line-number))
      (narrow-to-region (line-beginning-position) (line-end-position))
      (back-to-indentation))))

;;;###autoload
(defun prog-x-format-dwim ()
  (interactive "*")
  (let* ((use-region-p (use-region-p))
         (beg (if use-region-p (region-beginning) (point-min)))
         (end (if use-region-p (region-end) (point-max))))
    (prog-x-format-region beg end)))

(evil-define-operator prog-x-operator-format (beg end)
  :move-point nil
  (interactive "<r>")
  (prog-x-format-region beg end))

(defvar-local prog-x-eval-function nil)

(defun prog-x-eval-region (&optional beg end)
  (interactive "r")
  (unless prog-x-eval-function
    (error (format "porg-x-eval-region: major mode doesn't supported: %s"
                   (symbol-name major-mode))))
  (save-excursion
    (save-selected-window
      (funcall prog-x-eval-function beg end))))

(evil-define-operator prog-x-operator-eval (beg end)
  :move-point nil
  (interactive "<r>")
  (prog-x-eval-region beg end))

;;;###autoload
(defun prog-x-setup ()
  (define-key evil-normal-state-map "g=" #'prog-x-operator-format)
  (define-key evil-motion-state-map "gy" #'prog-x-operator-eval))

(provide 'prog-x)
