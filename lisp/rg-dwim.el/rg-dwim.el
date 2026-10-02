;;; rg-dwim.el --- Search with ripgrep smartly -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: tools, matching

;;; Commentary:

;; Search with ripgrep in a `grep-mode' buffer.  Call `rg-dwim'.

;;; Code:

(require 'grep)
(require 'project)

(defvar rg-dwim-program "rg"
  "Program of ripgrep.")

;;;###autoload
(defun rg-dwim (&optional arg)
  "RG dwim.
Without universal ARG, rg in project directory.
With one universal ARG, prompt for rg directory.
With two universal ARG, edit rg command."
  (interactive "P")
  (let* ((default-directory (if arg
                                (read-directory-name "Search directory: ")
                              (if-let* ((project (project-current)))
                                  (project-root project)
                                default-directory)))
         (pattern-default (thing-at-point 'symbol))
         (pattern-prompt (if pattern-default
                             (format "Search pattern (%s): " pattern-default)
                           "Search pattern: "))
         (pattern (read-regexp pattern-prompt pattern-default))
         (command-default (format "%s -n --no-heading --color=always -S %s ." rg-dwim-program pattern))
         (command (if (> (prefix-numeric-value arg) 4)
                      (read-string "Search command: " command-default 'grep-history)
                    command-default)))
    (grep--save-buffers)
    (compilation-start command 'grep-mode)))

(provide 'rg-dwim)
;;; rg-dwim.el ends here
