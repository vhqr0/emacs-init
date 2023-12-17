;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))


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


;;; dirty directories

(declare-variable! tramp
  tramp-backup-directory-alist)

(defun init--emacs-file-name-transforms (x)
  `((".*" ,(init--expand-emacs-file-name x) t)))

(defun init--emacs-directory-alist (x)
  `((".*" . ,(init--expand-emacs-file-name x))))

(setq auto-save-file-name-transforms (init--emacs-file-name-transforms "auto-save/")
      lock-file-name-transforms      (init--emacs-file-name-transforms "lock/"     )
      backup-directory-alist         (init--emacs-directory-alist      "backup/"   )
      tramp-backup-directory-alist   (init--emacs-directory-alist      "backup/"   )
      trash-directory                (init--expand-emacs-file-name     "trash/"    ))


;;; auto save visited

(defvar! init--auto-save-visited-predicate-hook nil)

(defun init--auto-save-visited-predicate ()
  (not (run-hook-with-args-until-success 'init--auto-save-visited-predicate-hook)))

(setq auto-save-visited-interval 1
      auto-save-visited-predicate #'init--auto-save-visited-predicate)

(after-init!
 (auto-save-visited-mode 1))

(add-to-list 'minor-mode-alist '(auto-save-visited-mode " AS"))

(defmacro define-auto-save-visited-predicate! (name &rest body)
  (declare (indent 1))
  (let ((name (intern (concat "init--auto-save-visited-predicate-for-" (symbol-name name)))))
    `(defun-add-hook! init--auto-save-visited-predicate-hook ,name ()
       ,@body)))


;;; recent files

(setq-declare! recentf
  recentf-max-saved-items 200)

(after-init!
 (recentf-mode 1))


;;; edit

(setq-default
 indent-tabs-mode nil
 truncate-lines t)

(setq system-time-locale "C")

(setq word-wrap-by-category t)

(setq disabled-command-function nil)

(global-set-key! "C-SPC" #'toggle-input-method)

(after-init!
 (repeat-mode 1))

(global-set-key! "M-o" #'embark-act)


;;; pairs

(defun init--insert-pair-1 (&optional arg open close)
  (interactive "P")
  (insert-pair (or arg '(1)) open close))

(setq-declare! paren
  show-paren-context-when-offscreen t)

(show-paren-mode 1)

(electric-pair-mode 1)

(add-hook! prog-mode rainbow-delimiters-mode)


;;; lines

(setq-declare! display-line-numbers
  display-line-numbers-type 'relative)

(setq-declare! hl-line
  global-hl-line-sticky-flag t)

(add-hook! (text-mode prog-mode) display-line-numbers-mode)

(setq-declare! page-break-lines
  page-break-lines-char ?-)

(after-init!
 (global-page-break-lines-mode 1))

(after-load! page-break-lines
  (diminish! page-break-lines))


;;; undo tree

(setq-declare! undo-tree
  undo-tree-mode-lighter nil
  undo-tree-auto-save-history nil)

(after-init!
 (global-undo-tree-mode 1))

(declare-variable! undo-tree
  undo-tree-visualizer-parent-buffer)

(define-auto-save-visited-predicate! undo-tree
  (and (bound-and-true-p undo-tree-mode)
       (let ((current-buffer (current-buffer)))
         (with-current-buffer (window-buffer)
           (and (eq major-mode 'undo-tree-visualizer-mode)
                (eq current-buffer undo-tree-visualizer-parent-buffer))))))


;;; completion

(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)


;;; isearch

(setq isearch-lazy-count t
      isearch-allow-scroll t
      isearch-allow-motion t
      isearch-yank-on-move t
      isearch-motion-changes-direction t
      isearch-repeat-on-direction-change t)


;;; helpful

(setq-declare! helpful
  helpful-max-buffers nil)



(setq inhibit-startup-screen t
      initial-scratch-message nil)

(setq use-dialog-box nil
      use-file-dialog nil)

(defvar!
 init--ui-disable-modes
 '(blink-cursor-mode tooltip-mode tool-bar-mode menu-bar-mode scroll-bar-mode))

(dolist (mode init--ui-disable-modes)
  (when (fboundp mode)
    (funcall mode -1)))


;;; themes

(defvar! init--load-theme-after-init t)

(defun init--load-theme (theme)
  (interactive
   (list (intern (completing-read "Load custom theme: "
                                  (mapcar #'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (message "load theme: %s" theme))

(defun init--load-theme-random ()
  (interactive)
  (init--load-theme (seq-random-elt (custom-available-themes))))

(when init--load-theme-after-init
  (after-init!
   (init--load-theme-random)))


;;; tabs

(setq tab-bar-tab-hints t
      tab-bar-select-tab-modifiers '(meta))

(global-set-key!
 "C-S-T" #'tab-bar-new-tab
 "C-S-W" #'tab-bar-close-tab)

(define-key! tab-prefix "`" #'toggle-frame-tab-bar)


;;; winner

(after-init!
 (winner-mode 1))

(provide 'init-basic-emacs)
