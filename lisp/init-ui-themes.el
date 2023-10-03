;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(defun init--themes-load-random ()
  (interactive)
  (let* ((themes (custom-available-themes))
         (theme (seq-random-elt themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)
    (message "load theme: %s" theme)))

(add-hook! after-init init--themes-load-random)

(define-key! help "te" #'init--themes-load-random)

(provide 'init-ui-themes)
