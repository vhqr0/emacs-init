;;; -*- lexical-binding: t; no-native-compile: t -*-

(defun xdg-open-file-linux (file)
  (call-process-shell-command (concat "xdg-open " (expand-file-name file))))

(defun xdg-open-file-windows (file)
  (call-process-shell-command (concat "explorer.exe file://" (expand-file-name file))))



(defvar xdg-open-file-function (if (eq system-type 'windows-nt)
                                   #'xdg-open-file-windows
                                 #'xdg-open-file-linux))

(declare-function dired-get-marked-files "dired")

;;;###autoload
(defun xdg-open (&optional files)
  (interactive)
  (let* ((files (cond (files)
                      (buffer-file-name)
                      ((eq major-mode 'dired-mode)
                       (dired-get-marked-files))
                      (default-directory)))
         (files (if (listp files) files (list files))))
    (dolist (file files)
      (funcall xdg-open-file-function file))))

(provide 'xdg-open)
