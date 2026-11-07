;;; search-x.el --- Tools for occur and compilation buffers -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: convenience

;;; Commentary:

;; Tools to revert or edit the arguments of occur and compilation
;; buffers, by clickable header line buttons and commands:
;;
;; - occur: Revert, EditRegexp, EditBuffer.
;; - compilation: Revert, EditCommand, EditDirectory.
;;
;;   (add-hook 'occur-mode-hook #'search-x-occur-header-setup)
;;   (add-hook 'compilation-mode-hook #'search-x-compile-header-setup)
;;   (keymap-set occur-mode-map "/" #'search-x-occur-edit-regexp)
;;   (keymap-set occur-mode-map "C-c C-/" #'search-x-occur-edit-regexp)
;;   (keymap-set compilation-mode-map "/" #'search-x-compile-edit-command)
;;   (keymap-set compilation-mode-map "C-c C-/" #'search-x-compile-edit-command)
;;   (keymap-set compilation-minor-mode-map "/" #'search-x-compile-edit-command)
;;   (keymap-set compilation-minor-mode-map "C-c C-/" #'search-x-compile-edit-command)
;;
;; `compilation-mode-map' does not inherit `compilation-minor-mode-map',
;; which `grep-mode-map' inherits.
;;
;; `search-x-header-button' makes such a button from a label and a
;; command.

;;; Code:

(require 'replace)
(require 'compile)

(defun search-x-header-button (label command)
  "Return a header line button showing LABEL, running COMMAND on click."
  (propertize
   label
   'face 'mode-line-buffer-id
   'mouse-face 'mode-line-highlight
   'local-map (define-keymap "<header-line> <mouse-1>" command)))

(defvar search-x-header-revert-button
  (search-x-header-button "Revert" #'revert-buffer)
  "Header line button to revert the buffer.")

;;; occur

(defun search-x-occur-edit-regexp ()
  "Edit occur regexp."
  (interactive)
  (let ((regexp (read-string "Occur regexp: " (car occur-revert-arguments) 'regexp-history)))
    (setf (car occur-revert-arguments) regexp))
  (occur-revert-function nil nil))

(defun search-x-occur-edit-buffer ()
  "Edit occur buffer."
  (interactive)
  (let ((buffer (get-buffer (read-buffer "Occur buffer: " nil t))))
    (setq default-directory (buffer-local-value 'default-directory buffer))
    (setq occur-revert-arguments (list (car occur-revert-arguments) nil (list buffer))))
  (occur-revert-function nil nil))

(defvar search-x-occur-header-format
  (concat
   search-x-header-revert-button
   " "
   (search-x-header-button "EditRegexp" #'search-x-occur-edit-regexp)
   " "
   (search-x-header-button "EditBuffer" #'search-x-occur-edit-buffer))
  "Header line format of occur buffers.")

;;;###autoload
(defun search-x-occur-header-setup ()
  "Set header line of occur buffers."
  (setq header-line-format search-x-occur-header-format))

;;; compile

(defun search-x-compile-edit-command ()
  "Edit compile command."
  (interactive)
  (recompile t))

(defun search-x-compile-edit-directory ()
  "Edit compile directory."
  (interactive)
  (let ((directory (read-directory-name "Compile directory: ")))
    (setq default-directory directory)
    (setq compilation-directory directory))
  (apply #'compilation-start compilation-arguments))

(defvar search-x-compile-header-format
  (concat
   search-x-header-revert-button
   " "
   (search-x-header-button "EditCommand" #'search-x-compile-edit-command)
   " "
   (search-x-header-button "EditDirectory" #'search-x-compile-edit-directory))
  "Header line format of compilation buffers.")

;;;###autoload
(defun search-x-compile-header-setup ()
  "Set header line of compilation buffers."
  (setq header-line-format search-x-compile-header-format))

(provide 'search-x)
;;; search-x.el ends here
