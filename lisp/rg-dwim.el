;;; rg-dwim.el --- Search with ripgrep smartly -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: tools, matching

;;; Commentary:

;; Search with ripgrep, by `rg-dwim-program', in a `grep-mode' buffer,
;; defaulting to the symbol at point.  `rg-dwim' searches:
;;
;; - without prefix argument, in the project root;
;; - with one `C-u', in a directory read;
;; - with two `C-u', also editing the command.
;;
;; `rg-dwim-command' returns the command searching a pattern, and
;; `rg-dwim-search' runs a command in a directory, such as:
;;
;;   (rg-dwim-search (rg-dwim-command (shell-quote-argument "TODO")) "~/notes/")

;;; Code:

(require 'grep)
(require 'project)

(defvar rg-dwim-program "rg"
  "Program of ripgrep.")

(defun rg-dwim-command (pattern)
  "Return the command searching PATTERN, a shell argument, by ripgrep."
  (format "%s -n --no-heading --color=always -S %s ." rg-dwim-program pattern))

(defun rg-dwim-search (command directory)
  "Run COMMAND in DIRECTORY in a `grep-mode' buffer."
  (let ((default-directory directory))
    (grep--save-buffers)
    (compilation-start command 'grep-mode)))

;;;###autoload
(defun rg-dwim (&optional arg)
  "RG dwim.
Without universal ARG, rg in project directory.
With one universal ARG, prompt for rg directory.
With two universal ARG, edit rg command."
  (interactive "P")
  (let* ((directory (if arg
                        (read-directory-name "Search directory: ")
                      (if-let* ((project (project-current)))
                          (project-root project)
                        default-directory)))
         (pattern-default (thing-at-point 'symbol))
         (pattern-prompt (if pattern-default
                             (format "Search pattern (%s): " pattern-default)
                           "Search pattern: "))
         (pattern (read-regexp pattern-prompt pattern-default))
         (command-default (rg-dwim-command pattern))
         (command (if (> (prefix-numeric-value arg) 4)
                      (read-string "Search command: " command-default 'grep-history)
                    command-default)))
    (rg-dwim-search command directory)))

(provide 'rg-dwim)
;;; rg-dwim.el ends here
