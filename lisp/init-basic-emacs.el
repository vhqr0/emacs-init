;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)


;;; essentials

(init-eval-after-init!
 (gcmh-mode 1))

(prefer-coding-system 'utf-8)

(setq system-time-locale "C")


;;; files

(setq vc-handled-backends '(Git)
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

(init-setq-declare!
 recentf-max-saved-items 200)

(init-eval-after-init!
 (recentf-mode 1))


;;; dirty directories

(defvar tramp-backup-directory-alist)

(defun init-emacs-file-name-transforms (x)
  `((".*" ,(init-expand-emacs-file-name x) t)))

(defun init-emacs-directory-alist (x)
  `((".*" . ,(init-expand-emacs-file-name x))))

(setq auto-save-file-name-transforms (init-emacs-file-name-transforms "auto-save/")
      lock-file-name-transforms      (init-emacs-file-name-transforms "lock/")
      backup-directory-alist         (init-emacs-directory-alist      "backup/")
      tramp-backup-directory-alist   (init-emacs-directory-alist      "backup/")
      trash-directory                (init-expand-emacs-file-name     "trash/"))


;;; auto save visited

(defvar init-auto-save-visited-predicate-hook nil)

(defun init-auto-save-visited-predicate ()
  ;; block auto save (return nil) if satisfy any predicates
  (not (run-hook-with-args-until-success 'init-auto-save-visited-predicate-hook)))

(setq auto-save-visited-interval 1
      auto-save-visited-predicate #'init-auto-save-visited-predicate)

(init-eval-after-init!
 (auto-save-visited-mode 1))

(add-to-list 'minor-mode-alist '(auto-save-visited-mode " AS"))


;;; undo tree

(init-setq-declare!
 undo-tree-mode-lighter nil
 undo-tree-auto-save-history nil)

(init-eval-after-init!
 (global-undo-tree-mode 1))

(defvar undo-tree-visualizer-parent-buffer)

(defun init-auto-save-visited-predicate-for-undo-tree ()
  (and (bound-and-true-p undo-tree-mode)
       (let ((current-buffer (current-buffer)))
         (with-current-buffer (window-buffer)
           (and (eq major-mode 'undo-tree-visualizer-mode)
                (eq current-buffer undo-tree-visualizer-parent-buffer))))))

(init-add-hook 'init-auto-save-visited-predicate-hook #'init-auto-save-visited-predicate-for-undo-tree)


;;; projectile

(init-setq-declare!
 projectile-current-project-on-switch 'move-to-end)

(init-eval-after-init!
 (projectile-mode 1))

(provide 'init-basic-emacs)
