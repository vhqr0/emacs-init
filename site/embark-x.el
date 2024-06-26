;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'embark)
(require 'which-key)

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the current
target followed by an ellipsis if there are further targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read
prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

(defun embark-which-key-enable ()
  (interactive)
  (advice-add 'embark-mixed-indicator :override #'embark-which-key-indicator)
  (advice-add 'embark-completing-read-prompter :around #'embark-hide-which-key-indicator))

(defun embark-which-key-disable ()
  (interactive)
  (advice-remove 'embark-mixed-indicator #'embark-which-key-indicator)
  (advice-remove 'embark-completing-read-prompter #'embark-hide-which-key-indicator))

(provide 'embark-x)
