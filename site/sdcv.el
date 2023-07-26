;;; -*- lexical-binding: t; no-native-compile: t -*-

(defvar sdcv-program "sdcv")
(defvar sdcv-option "-n")

;;;###autoload
(defun sdcv (&optional word)
  (interactive (list (if current-prefix-arg
                         (read-string "sdcv: ")
                       (thing-at-point 'word))))
  (when word
    (shell-command (format "%s %s %s" sdcv-program sdcv-option word)
                   (get-buffer-create "*sdcv*"))))

(provide 'sdcv)
