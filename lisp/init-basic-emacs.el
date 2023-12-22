;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)


;;; essentials

(setq system-time-locale "C")

(setq inhibit-startup-screen t
      initial-scratch-message nil)

(setq use-dialog-box nil
      use-file-dialog nil)

(setq ring-bell-function #'ignore)

(setq disabled-command-function nil)

(setq word-wrap-by-category t)

(init-setq-declare!
 helpful-max-buffers nil)

(init-global-set-key "C-SPC" #'toggle-input-method)

(init-global-set-key "M-o" #'embark-act)

(init-eval-after-init!
 (gcmh-mode 1))


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


;;; edit

(setq-default
 indent-tabs-mode nil
 truncate-lines t)

(init-eval-after-init!
 (repeat-mode 1))

(defun init-insert-pair-1 (&optional arg open close)
  (interactive "P")
  (insert-pair (or arg '(1)) open close))

(init-setq-declare!
 sp-ignore-modes-list nil
 sp-base-key-bindings 'paredit
 sp-paredit-bindings '(("C-M-f"       . sp-forward-sexp)
                       ("C-M-b"       . sp-backward-sexp)
                       ("C-M-d"       . sp-down-sexp)
                       ("C-M-u"       . sp-backward-up-sexp)
                       ("C-M-n"       . sp-next-sexp)
                       ("C-M-p"       . sp-previous-sexp)
                       ("C-M-k"       . sp-kill-sexp)
                       ("C-M-w"       . sp-copy-sexp)
                       ("C-M-SPC"     . sp-mark-sexp)
                       ("C-k"         . sp-kill-hybrid-sexp)
                       ("M-r"         . sp-splice-sexp-killing-around)
                       ("M-R"         . sp-splice-sexp-killing-backward)
                       ("M-s"         . sp-splice-sexp)
                       ("M-S"         . sp-split-sexp)
                       ("M-J"         . sp-join-sexp)
                       ("M-?"         . sp-convolute-sexp)
                       ("C-<right>"   . sp-forward-slurp-sexp)
                       ("C-<left>"    . sp-forward-barf-sexp)
                       ("C-M-<left>"  . sp-backward-slurp-sexp)
                       ("C-M-<right>" . sp-backward-barf-sexp)))

(with-eval-after-load 'smartparens
  (require 'smartparens-config)
  (init-diminish-minor-mode 'smartparens-mode))

(init-eval-after-init!
 (smartparens-global-mode 1)
 (show-smartparens-global-mode 1))

(init-add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(init-setq-declare!
 global-hl-line-sticky-flag t)

(defun init-toggle-line-numbers-type ()
  (interactive)
  (setq-local display-line-numbers-type (if (eq display-line-numbers-type 'relative) t 'relative))
  (display-line-numbers-mode 1))

(init-add-hook '(text-mode-hook prog-mode-hook) #'display-line-numbers-mode)

(init-setq-declare!
 page-break-lines-char ?-)

(init-eval-after-init!
 (global-page-break-lines-mode 1))

(with-eval-after-load 'page-break-lines
  (init-diminish-minor-mode 'page-break-lines-mode))


;;; completion

(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

(setq isearch-lazy-count t
      isearch-allow-scroll t
      isearch-allow-motion t
      isearch-yank-on-move t
      isearch-motion-changes-direction t
      isearch-repeat-on-direction-change t)

(init-global-set-key "M-/" #'hippie-expand)


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


;;; ui

(defvar init-ui-disabled-modes
  '(blink-cursor-mode tooltip-mode tool-bar-mode menu-bar-mode scroll-bar-mode))

(dolist (mode init-ui-disabled-modes)
  (when (fboundp mode)
    (funcall mode -1)))

(defvar init-load-theme-after-init t)

(defun init-load-theme (theme)
  (interactive
   (list (intern (completing-read "Load custom theme: "
                                  (mapcar #'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (message "load theme: %s" theme))

(defun init-load-theme-random ()
  (interactive)
  (init-load-theme (seq-random-elt (custom-available-themes))))

(when init-load-theme-after-init
  (init-eval-after-init!
   (init-load-theme-random)))


;;; windows

(setq tab-bar-tab-hints t
      tab-bar-select-tab-modifiers '(meta))

(init-global-set-key
 "C-S-T" #'tab-bar-new-tab
 "C-S-W" #'tab-bar-close-tab)

(init-define-key tab-prefix-map "`" #'toggle-frame-tab-bar)

(init-eval-after-init!
 (winner-mode 1))

(provide 'init-basic-emacs)
