;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'cl-lib)
(require 'eshell)

(defvar eshell-buffer-name)
(declare-function eshell-reset "esh-mode")
(declare-function eshell/cd "em-dirs")

(declare-function project-current "project")
(declare-function project-root "project")
(declare-function project-prefixed-buffer-name "project")

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
           (let ((parent (window-parent (selected-window))))
             (cond ((window-left-child parent)
                    (select-window (split-window-vertically))
                    (switch-to-buffer buffer))
                   ((window-top-child parent)
                    (select-window (split-window-horizontally))
                    (switch-to-buffer buffer))
                   (t
                    (switch-to-buffer-other-window buffer))))))))

(defun project-eshell-dwim (&optional arg)
  (interactive "P")
  (let* ((project (project-current))
         (default-directory (if project (project-root project) default-directory))
         (eshell-buffer-name (project-prefixed-buffer-name "eshell")))
    (eshell-dwim arg)))

(provide 'eshell-x)
