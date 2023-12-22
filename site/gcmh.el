;;; -*- lexical-binding: t; no-native-compile: t -*-

(defvar gcmh-idle-timer nil)
(defvar gcmh-idle-delay 15)
(defvar gcmh-low-cons-threshold 800000)
(defvar gcmh-high-cons-threshold #x40000000)

(defun gcmh-set-low-threshold ()
  (setq gc-cons-threshold gcmh-low-cons-threshold))

(defun gcmh-set-high-threshold ()
  (setq gc-cons-threshold gcmh-high-cons-threshold))

(defun gcmh-register-timer ()
  (when (timerp gcmh-idle-timer)
    (cancel-timer gcmh-idle-timer))
  (setq gcmh-idle-timer (run-with-timer gcmh-idle-delay nil #'gcmh-set-low-threshold)))

;;;###autoload
(define-minor-mode gcmh-mode
  "Minor mode to tweak Garbage Collection strategy."
  :group 'alloc
  :global t
  (if gcmh-mode
      (progn
        (gcmh-set-high-threshold)
	(add-hook 'pre-command-hook #'gcmh-set-high-threshold)
	(add-hook 'post-command-hook #'gcmh-register-timer))
    (gcmh-set-low-threshold)
    (setq gcmh-idle-timer nil)
    (remove-hook 'pre-command-hook #'gcmh-set-high-threshold)
    (remove-hook 'post-command-hook #'gcmh-register-timer)))

(provide 'gcmh)
