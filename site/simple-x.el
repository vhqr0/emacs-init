;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'thingatpt)

;;;###autoload
(defun minibuffer-yank-symbol ()
  (interactive)
  (when (window-minibuffer-p)
    (let ((symbol (with-selected-window (minibuffer-selected-window)
                    (thing-at-point 'symbol))))
      (when symbol
        (insert symbol)))))

;;;###autoload
(defun kill-buffer-dwim (&optional arg)
  (interactive "P")
  (cond (arg
         (call-interactively #'kill-buffer))
        ((memq (read-char (format "Kill current buffer <%s>? [y]" (buffer-name)))
               '(?\r ?y))
         (kill-buffer))
        (t
         (message "Aborted"))))

;;;###autoload
(defun simple-x-setup ()
  (define-key minibuffer-local-map (kbd "M-.") #'minibuffer-yank-symbol))

(provide 'simple-x)
