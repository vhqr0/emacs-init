;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'evil)

(defvar x-utils-xclip-program "xclip")
(defvar x-utils-xclip-option "-selection clip")

(defun x-utils-xclip-region (&optional beg end)
  (interactive "r")
  (call-shell-region
   beg end (format "%s %s" x-utils-xclip-program x-utils-xclip-option)))

(evil-define-operator x-utils-operator-xclip (beg end)
  :move-point nil
  (interactive "<r>")
  (x-utils-xclip-region beg end))

(defvar x-utils-open-program "xdg-open")
(defvar x-utils-open-option "")
(defvar x-utils-open-urlize nil)

(declare-function dired-get-marked-files "dired")

;;;###autoload
(defun x-utils-xdg-open (&optional files)
  (interactive)
  (let* ((files (cond (files)
                      (buffer-file-name)
                      ((eq major-mode 'dired-mode)
                       (dired-get-marked-files))
                      (default-directory)))
         (files (if (listp files) files (list files))))
    (dolist (file files)
      (when x-utils-open-urlize
        (setq file (concat "file://" (expand-file-name file))))
      (call-process-shell-command
       (format "%s %s %s" x-utils-open-program x-utils-open-option file)))))

;;;###autoload
(defun x-utils-setup ()
  (define-key evil-motion-state-map "gx" #'x-utils-operator-xclip)
  (define-key evil-normal-state-map "gx" #'x-utils-operator-xclip))

(provide 'x-utils)
