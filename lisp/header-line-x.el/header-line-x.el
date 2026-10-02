;;; header-line-x.el --- Header line buttons for special buffers -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: convenience

;;; Commentary:

;; Clickable header line buttons to revert or edit the arguments of occur
;; and compilation buffers.  Add `header-line-x-occur-setup' to
;; `occur-mode-hook' and `header-line-x-compile-setup' to
;; `compilation-mode-hook'.

;;; Code:

(require 'replace)
(require 'compile)

(defun header-line-x-button (label command)
  "Return a header line button showing LABEL, running COMMAND on click."
  (propertize
   label
   'face 'mode-line-buffer-id
   'mouse-face 'mode-line-highlight
   'local-map (define-keymap "<header-line> <mouse-1>" command)))

(defvar header-line-x-revert-button
  (header-line-x-button "Revert" #'revert-buffer)
  "Header line button to revert the buffer.")

;;; occur

(defun header-line-x-occur-edit-regexp ()
  "Edit occur regexp."
  (interactive)
  (let ((regexp (read-string "Occur regexp: " (car occur-revert-arguments) 'regexp-history)))
    (setf (car occur-revert-arguments) regexp))
  (occur-revert-function nil nil))

(defun header-line-x-occur-edit-buffer ()
  "Edit occur buffer."
  (interactive)
  (let ((buffer (get-buffer (read-buffer "Occur buffer: " nil t))))
    (setq default-directory (buffer-local-value 'default-directory buffer))
    (setq occur-revert-arguments (list (car occur-revert-arguments) nil (list buffer))))
  (occur-revert-function nil nil))

(defvar header-line-x-occur-format
  (concat
   header-line-x-revert-button
   " "
   (header-line-x-button "EditRegexp" #'header-line-x-occur-edit-regexp)
   " "
   (header-line-x-button "EditBuffer" #'header-line-x-occur-edit-buffer))
  "Header line format of occur buffers.")

;;;###autoload
(defun header-line-x-occur-setup ()
  "Set header line of occur buffers."
  (setq header-line-format header-line-x-occur-format))

;;; compile

(defun header-line-x-compile-edit-command ()
  "Edit compile command."
  (interactive)
  (recompile t))

(defun header-line-x-compile-edit-directory ()
  "Edit compile directory."
  (interactive)
  (let ((directory (read-directory-name "Compile directory: ")))
    (setq default-directory directory)
    (setq compilation-directory directory))
  (apply #'compilation-start compilation-arguments))

(defvar header-line-x-compile-format
  (concat
   header-line-x-revert-button
   " "
   (header-line-x-button "EditCommand" #'header-line-x-compile-edit-command)
   " "
   (header-line-x-button "EditDirectory" #'header-line-x-compile-edit-directory))
  "Header line format of compilation buffers.")

;;;###autoload
(defun header-line-x-compile-setup ()
  "Set header line of compilation buffers."
  (setq header-line-format header-line-x-compile-format))

(provide 'header-line-x)
;;; header-line-x.el ends here
