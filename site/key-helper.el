;;; -*- lexical-binding: t; no-native-compile: t -*-

(defvar key-helper-C-u-transient-map nil)

;;;###autoload
(defun key-helper-C-u ()
  (interactive)
  (setq prefix-arg
        (list (* 4 (if current-prefix-arg
                       (prefix-numeric-value current-prefix-arg)
                     1))))
  (when key-helper-C-u-transient-map
    (set-transient-map key-helper-C-u-transient-map)))

(defun key-helper-execute-key-binding (key-binding)
  (cond ((commandp key-binding)
         (setq this-command key-binding
               real-this-command key-binding)
         (if (commandp key-binding t)
             (call-interactively key-binding)
           (execute-kbd-macro key-binding))
         key-binding)
        ((keymapp key-binding)
         (set-transient-map key-binding)
         key-binding)))

(defun key-helper-god (prefix)
  (let ((chr (read-char (concat prefix " C-"))))
    (cond ((key-helper-execute-key-binding (key-binding (kbd (format "%s C-%c" prefix chr)))))
          ((key-helper-execute-key-binding (key-binding (kbd (format "%s C-%c" prefix chr)))))
          (t
           (error (format "key-helper-god: no key binding found on %s %c" prefix chr))))))

;;;###autoload
(defun key-helper-C-c ()
  (interactive)
  (key-helper-god "C-c"))

(provide 'init-basic-maps)
