;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(setq disabled-command-function nil)

(declare-variable! autorevert
  auto-revert-check-vc-info)

(declare-variable! undo-tree
  undo-tree-history-directory-alist)

(setq auto-revert-check-vc-info t
      vc-handled-backends '(Git)
      vc-make-backup-files t
      version-control t
      kept-old-versions 10
      kept-new-versions 10
      delete-old-versions t
      backup-by-copying t
      delete-by-moving-to-trash t
      remote-file-name-inhibit-locks t
      remote-file-name-inhibit-delete-by-moving-to-trash t
      remote-file-name-inhibit-auto-save-visited t)

(defun init--emacs-file-name-transforms (x)
  `((".*" ,(init--expand-emacs-file-name x) t)))

(defun init--emacs-directory-alist (x)
  `((".*" . ,(init--expand-emacs-file-name x))))

(setq auto-save-file-name-transforms    (init--emacs-file-name-transforms "auto-save/")
      lock-file-name-transforms         (init--emacs-file-name-transforms "lock/"     )
      backup-directory-alist            (init--emacs-directory-alist      "backup/"   )
      undo-tree-history-directory-alist (init--emacs-directory-alist      "undo-tree/")
      trash-directory                   (init--expand-emacs-file-name     "trash/"    ))

(comment! auto-save-visited
  (defvar! init--auto-save-visited-predicate-hook nil)

  (defun init--auto-save-visited-predicate ()
    (not (run-hook-with-args-until-success 'init--auto-save-visited-predicate-hook)))

  (defmacro define-auto-save-visited-predicate! (name &rest body)
    (declare (indent 1))
    (let ((name (intern (concat "init--auto-save-visited-predicate-for-" (symbol-name name)))))
      `(defun-add-hook! init--auto-save-visited-predicate-hook ,name ()
         ,@body)))

  (setq auto-save-visited-interval 1
        auto-save-visited-predicate #'init--auto-save-visited-predicate)

  (auto-save-visited-mode 1)
  (add-to-list 'minor-mode-alist '(auto-save-visited-mode " AS")))

(comment! recentf
  (setq-declare! recentf
    recentf-max-saved-items 200)
  (recentf-mode 1))

(comment! repeat
  (repeat-mode 1))

(comment! winner
  (winner-mode 1))

(comment! simple-x
  (simple-x-setup))

(comment! undo-tree
  (declare-variable! undo-tree
    undo-tree-visualizer-parent-buffer)

  (setq-declare! undo-tree
    undo-tree-mode-lighter nil)

  (global-undo-tree-mode 1)

  (define-auto-save-visited-predicate! undo-tree
    (and (bound-and-true-p undo-tree-mode)
         (let ((current-buffer (current-buffer)))
           (with-current-buffer (window-buffer)
             (and (eq major-mode 'undo-tree-visualizer-mode)
                  (eq current-buffer undo-tree-visualizer-parent-buffer)))))))

(provide 'init-basic-emacs)
