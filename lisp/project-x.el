;;; project-x.el --- Tools for projects -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: convenience

;;; Commentary:

;; Tools for projects:
;;
;; - `project-x-switch-to-compile' switches to the compilation buffer of
;;   the current project, as named by `project-compile'.
;;
;;   (keymap-set project-prefix-map "C" #'project-x-switch-to-compile)

;;; Code:

(require 'project)
(require 'compile)

;;;###autoload
(defun project-x-switch-to-compile ()
  "Switch to the compilation buffer of the current project."
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (if-let* ((buffer (get-buffer (compilation-buffer-name "compilation" nil nil))))
        (switch-to-buffer buffer)
      (user-error "No compilation buffer of the project"))))

(provide 'project-x)
;;; project-x.el ends here
