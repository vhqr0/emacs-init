;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'helm)
(require 'bufler)
(require 'bufler-workspace)

(defun helm-bufler-switch-buffer (buffer)
  "Switch to BUFFER.
With two universal prefixes, also set the frame's workspace.
This mimics `bufler-workspace-switch-buffer'."
  (when (equal '(16) current-prefix-arg)
    (bufler-workspace-set
     (bufler-buffer-workspace-path buffer)))
  (switch-to-buffer buffer))

(defvar helm-bufler-source
  (helm-make-source "Bufler's workspace buffers" 'helm-source-sync
    :header-name (lambda (_name)
                   (concat "Bufler"
                           (unless current-prefix-arg
                             (concat ": " (bufler-format-path (frame-parameter nil 'bufler-workspace-path))))))
    :candidates (lambda ()
                  (let* ((bufler-vc-state nil)
                         (group-path (unless current-prefix-arg
                                       (frame-parameter nil 'bufler-workspace-path))))
                    (pcase current-prefix-arg
                      ((or `nil '(4) '(16))
                       (bufler-buffer-alist-at
                        group-path :filter-fns bufler-workspace-switch-buffer-filter-fns))
                      (_ (bufler-buffer-alist-at nil)))))
    :action (cons (cons "Switch to buffer with Bufler" 'helm-bufler-switch-buffer)
                  helm-type-buffer-actions))
  "Helm source for `bufler'.")

(provide 'helm-bufler)
