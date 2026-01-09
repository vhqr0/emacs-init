;;; init-emacs.el --- Init Emacs -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Emacs itself.

(require 'init-core)

;;; Code:

;;; evil

(defvar evil-want-keybinding)
(setq evil-want-keybinding nil)

(defvar evil-want-minibuffer)
(setq evil-want-minibuffer t)

(defvar evil-insert-state-bindings)
(setq evil-insert-state-bindings nil)

(defvar evil-want-C-i-jump)
(defvar evil-want-C-u-scroll)
(defvar evil-want-Y-yank-to-eol)
(setq evil-want-C-i-jump nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-Y-yank-to-eol t)

(defvar evil-respect-visual-line-mode)
(setq evil-respect-visual-line-mode t)

(defvar evil-search-module)
(setq evil-search-module 'evil-search)

(defvar evil-undo-system)
(setq evil-undo-system 'undo-redo)

(require 'evil)
(require 'evil-surround)

(evil-mode 1)
(global-evil-surround-mode 1)

(setq evil-mode-line-format '(before . mode-line-front-space))

(setq evil-want-fine-undo t)
(setq evil-symbol-word-search t)

(setq evil-goto-definition-functions
      '(evil-goto-definition-imenu evil-goto-definition-xref))

(setq evil-emacs-state-modes nil)
(setq evil-insert-state-modes nil)
(setq evil-motion-state-modes '(special-mode))

(setq-default evil-surround-pairs-alist
              '((?\( . ("( " . " )"))
                (?\{ . ("{ " . " }"))
                (?\[ . ("[ " . " ]"))
                (?\< . ("< " . " >"))
                (?\) . ("(" . ")"))
                (?\} . ("{" . "}"))
                (?\] . ("[" . "]"))
                (?\> . ("<" . ">"))
                (?b  . ("(" . ")"))
                (?B  . ("{" . "}"))
                (?r  . ("[" . "]"))
                (?a  . ("<" . ">"))
                (?#  . ("#{" . "}"))
                (?t  . evil-surround-read-tag)
                (?f  . evil-surround-prefix-function)))

(evil-define-operator init-evil-operator-comment (beg end)
  :move-point nil
  (interactive "<r>")
  (comment-or-uncomment-region beg end))

(evil-define-operator init-evil-operator-narrow (beg end)
  :move-point nil
  (interactive "<r>")
  (narrow-to-region beg end))

(defvar init-evil-eval-function-alist nil)

(evil-define-operator init-evil-operator-eval (beg end)
  :move-point nil
  (interactive "<r>")
  (when-let* ((eval-function (cdr (assq major-mode init-evil-eval-function-alist))))
    (funcall eval-function beg end)))

(evil-define-text-object init-evil-inner-line (count &optional _beg _end _type)
  (evil-range
   (save-excursion
     (back-to-indentation)
     (point))
   (line-end-position)
   'exclusive))

(evil-define-text-object init-evil-a-line (count &optional _beg _end _type)
  (evil-range (line-beginning-position) (line-end-position) 'inclusive))

(evil-define-text-object init-evil-inner-defun (count &optional beg end _type)
  (evil-select-inner-object 'evil-defun beg end type count t))

(evil-define-text-object init-evil-a-defun (count &optional beg end _type)
  (evil-select-an-object 'evil-defun beg end type count t))

(evil-define-text-object init-evil-an-entire (count &optional _beg _end _type)
  (evil-range (point-min) (point-max) 'line))

(set-keymap-parent evil-command-line-map minibuffer-local-map)

(keymap-unset evil-normal-state-map "<remap> <yank-pop>" t)

(keymap-unset evil-motion-state-map "RET" t)
(keymap-unset evil-motion-state-map "SPC" t)
(keymap-unset evil-normal-state-map "DEL" t)

(keymap-set evil-motion-state-map "<left>" #'evil-scroll-left)
(keymap-set evil-motion-state-map "<right>" #'evil-scroll-right)
(keymap-set evil-motion-state-map "<up>" #'evil-scroll-up)
(keymap-set evil-motion-state-map "<down>" #'evil-scroll-down)

(keymap-set evil-insert-state-map "C-r" #'evil-paste-from-register)
(keymap-set evil-insert-state-map "C-o" #'evil-execute-in-normal-state)

(keymap-unset evil-motion-state-map "<down-mouse-1>")
(keymap-unset evil-visual-state-map "<mouse-2>")
(keymap-unset evil-normal-state-map "<mouse-2>")

(keymap-set evil-motion-state-map "C-q" #'evil-record-macro)
(keymap-set evil-motion-state-map "q" #'quit-window)
(keymap-unset evil-normal-state-map "q" t)

(keymap-set evil-visual-state-map "m" #'evil-jump-item)
(keymap-set evil-operator-state-map "m" #'evil-jump-item)
(keymap-set evil-operator-state-map "o" #'evil-inner-symbol)
(keymap-set evil-operator-state-map "p" #'evil-inner-paragraph)

(keymap-set evil-normal-state-map "g c" #'init-evil-operator-comment)
(keymap-set evil-motion-state-map "g -" #'init-evil-operator-narrow)
(keymap-set evil-motion-state-map "g y" #'init-evil-operator-eval)

(keymap-set evil-inner-text-objects-map "r" #'evil-inner-bracket)
(keymap-set evil-outer-text-objects-map "r" #'evil-a-bracket)
(keymap-set evil-inner-text-objects-map "a" #'evil-inner-angle)
(keymap-set evil-outer-text-objects-map "a" #'evil-an-angle)
(keymap-set evil-inner-text-objects-map "l" #'init-evil-inner-line)
(keymap-set evil-outer-text-objects-map "l" #'init-evil-a-line)
(keymap-set evil-inner-text-objects-map "d" #'init-evil-inner-defun)
(keymap-set evil-outer-text-objects-map "d" #'init-evil-a-defun)
(keymap-set evil-inner-text-objects-map "h" #'init-evil-an-entire)
(keymap-set evil-outer-text-objects-map "h" #'init-evil-an-entire)

(keymap-set evil-motion-state-map "g f" 'find-file-at-point)
(keymap-set evil-motion-state-map "] f" 'find-file-at-point)
(keymap-set evil-motion-state-map "[ f" 'find-file-at-point)
(keymap-set evil-motion-state-map "g F" 'evil-find-file-at-point-with-line)
(keymap-set evil-motion-state-map "] F" 'evil-find-file-at-point-with-line)
(keymap-set evil-motion-state-map "[ F" 'evil-find-file-at-point-with-line)

(defvar init-evil-adjust-cursor-ignore-commands
  '(forward-sexp forward-list))

(define-advice evil-adjust-cursor (:before-until (&rest _) ignore-commands)
  (memq this-command init-evil-adjust-cursor-ignore-commands))

(defun init-evil-search-clean ()
  "Delete `evil-ex-search' persistent highlight."
  (evil-ex-delete-hl 'evil-ex-search))

(defvar init-evil-search-clean-idle 1.5)
(defvar init-evil-search-clean-timer nil)

(defun init-evil-search-clean-start-timer ()
  "Start a daemon to idle clean `evil-ex-search' persistent highlight."
  (unless init-evil-search-clean-timer
    (setq init-evil-search-clean-timer
          (run-with-idle-timer init-evil-search-clean-idle t #'init-evil-search-clean))))

(add-hook 'after-init-hook #'init-evil-search-clean-start-timer)

(defun init-evil-keymap-set (state keymap &rest clauses)
  "Set evil key.
STATE KEYMAP CLAUSES see `evil-define-key*'."
  (declare (indent defun))
  (apply #'evil-define-key* state keymap
         (seq-map-indexed
          (lambda (v i)
            (if (cl-oddp i) v (kbd v)))
          clauses)))

;;; initial

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

(defvar init-disable-ui-modes
  '(blink-cursor-mode tool-bar-mode menu-bar-mode scroll-bar-mode))

(defun init-disable-ui ()
  "Disable various ui modes."
  (interactive)
  (dolist (mode init-disable-ui-modes)
    (when (fboundp mode)
      (funcall mode -1))))

(add-hook 'after-init-hook #'init-disable-ui)

;;; files

(setq column-number-mode t)
(setq mode-line-percent-position '(6 "%q"))
(setq mode-line-position-line-format '(" %lL"))
(setq mode-line-position-column-format '(" %CC"))
(setq mode-line-position-column-line-format '(" %l:%C"))

(setq version-control t)
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq delete-by-moving-to-trash t)

(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "save/" user-emacs-directory) t)))
(setq lock-file-name-transforms      `((".*" ,(expand-file-name "lock/" user-emacs-directory) t)))
(setq backup-directory-alist         `((".*" . ,(expand-file-name "backup/" user-emacs-directory))))

(keymap-set ctl-x-x-map "G" #'revert-buffer)

(keymap-set evil-motion-state-map "g r" #'revert-buffer-quick)
(keymap-set evil-motion-state-map "g R" #'revert-buffer)

(keymap-set ctl-x-x-map "<left>" #'previous-buffer)
(keymap-set ctl-x-x-map "<right>" #'next-buffer)

(defun init-kill-current-buffer ()
  "Confirm then kill current buffer."
  (interactive)
  (when (y-or-n-p "Kill current buffer?")
    (kill-current-buffer)))

(defun init-auto-save-p ()
  "Predication of `auto-save-visited-mode'."
  (not (evil-insert-state-p)))

(setq auto-save-visited-interval 0.5)
(setq auto-save-visited-predicate #'init-auto-save-p)
(add-to-list 'minor-mode-alist '(auto-save-visited-mode " ASave"))
(add-hook 'after-init-hook #'auto-save-visited-mode)

(require 'autorevert)
(setq auto-revert-check-vc-info t)
(add-hook 'after-init-hook #'global-auto-revert-mode)

(require 'recentf)
(setq recentf-max-saved-items 500)
(add-hook 'after-init-hook #'recentf-mode)
(keymap-set ctl-x-r-map "e" #'recentf-open)

;;;; vc

(require 'vc)
(require 'vc-git)

(setq vc-handled-backends '(Git))
(setq vc-display-status 'no-backend)
(setq vc-make-backup-files t)

(keymap-set ctl-x-x-map "v" #'vc-refresh-state)

(defvar init-git-user-name "vhqr0")
(defvar init-git-user-email "zq_cmd@163.com")

(defun init-git-config-user ()
  "Init git repo."
  (interactive)
  (let ((directory default-directory)
        (buffer (get-buffer-create "*git-config*")))
    (save-window-excursion
      (with-current-buffer buffer
        (setq default-directory directory)
        (erase-buffer)
        (async-shell-command
         (format "%s config --local user.name %s && %s config --local user.email %s"
                 vc-git-program init-git-user-name vc-git-program init-git-user-email)
         (current-buffer))))))

;;;; project

(require 'project)

(setq project-mode-line t)
(setq project-switch-use-entire-map t)
(setq project-compilation-buffer-name-function #'project-prefixed-buffer-name)

(defvar-local init-find-test-file-function nil)

(defun init-project-find-test-file ()
  "Find test file in this project."
  (interactive)
  (if (not init-find-test-file-function)
      (user-error "No find test file function found")
    (let ((default-directory (project-root (project-current t))))
      (funcall init-find-test-file-function))))

(keymap-set project-prefix-map "t" #'init-project-find-test-file)

;;; window

(defun init-toggle-scroll-bar ()
  "Toggle scroll bar."
  (interactive)
  (if (or scroll-bar-mode horizontal-scroll-bar-mode)
      (progn
        (scroll-bar-mode -1)
        (horizontal-scroll-bar-mode -1))
    (scroll-bar-mode 1)
    (horizontal-scroll-bar-mode 1)))

(require 'windmove)
(windmove-default-keybindings)
(undelete-frame-mode 1)

(require 'tab-bar)

(setq tab-bar-position t)
(setq tab-bar-tab-hints t)
(setq tab-bar-select-tab-modifiers '(control meta))
(setq tab-bar-close-last-tab-choice 'delete-frame)

(tab-bar-mode 1)
(tab-bar-history-mode 1)

(defvar-keymap init-tab-bar-history-repeat-map
  :repeat t
  "<left>" #'tab-bar-history-back
  "<right>" #'tab-bar-history-forward)

(keymap-set tab-prefix-map "<left>" #'tab-bar-history-back)
(keymap-set tab-prefix-map "<right>" #'tab-bar-history-forward)

(keymap-set evil-window-map "<left>" #'tab-bar-history-back)
(keymap-set evil-window-map "<right>" #'tab-bar-history-forward)

(keymap-global-set "C-S-N" #'make-frame-command)
(keymap-global-set "C-S-T" #'tab-bar-new-tab)
(keymap-global-set "C-S-W" #'tab-bar-close-tab)

(keymap-global-set "C-0" #'text-scale-adjust)
(keymap-global-set "C--" #'text-scale-adjust)
(keymap-global-set "C-+" #'text-scale-adjust)
(keymap-global-set "C-=" #'text-scale-adjust)
;; C-M-0 is used by tab-bar
;; (keymap-global-set "C-M-0" #'global-text-scale-adjust)
(keymap-global-set "C-M--" #'global-text-scale-adjust)
(keymap-global-set "C-M-+" #'global-text-scale-adjust)
(keymap-global-set "C-M-=" #'global-text-scale-adjust)

;;; edit

(setq ring-bell-function #'ignore)
(setq disabled-command-function nil)
(setq suggest-key-bindings nil)
(setq word-wrap-by-category t)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)

(require 'hl-line)
(require 'display-line-numbers)

(defun init-toggle-line-numbers-relative ()
  "Toggle local display type of line numbers."
  (interactive)
  (setq-local display-line-numbers-type
              (if (eq display-line-numbers-type 'relative)
                  t
                'relative))
  (display-line-numbers-mode 1))

(defun init-set-line-modes ()
  "Set line modes."
  (setq-local show-trailing-whitespace t)
  (hl-line-mode 1)
  (display-line-numbers-mode 1))

(add-hook 'text-mode-hook #'init-set-line-modes)
(add-hook 'prog-mode-hook #'init-set-line-modes)

(defun init-indent-dwim ()
  "Do indent smartly."
  (interactive "*")
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (indent-region (point-min) (point-max))))

(require 'repeat)
(add-hook 'after-init-hook #'repeat-mode)

(require 'embark)
(keymap-global-set "M-o" #'embark-act)
(keymap-global-set "M-O" #'embark-act-all)
(keymap-set embark-identifier-map "%" #'query-replace)

;;;; paredit

(require 'elec-pair)
(electric-pair-mode 1)

(require 'paren)
;; (setq show-paren-style 'expression)
(setq show-paren-context-when-offscreen 'child-frame)
(show-paren-mode 1)

(require 'paredit)
(keymap-global-set "M-r" #'raise-sexp)
(keymap-global-set "M-R" #'paredit-splice-sexp-killing-backward)
(keymap-global-set "M-K" #'paredit-splice-sexp-killing-forward)
(keymap-global-set "M-s" #'paredit-splice-sexp)
(keymap-global-set "M-S" #'paredit-split-sexp)
(keymap-global-set "M-J" #'paredit-join-sexps)
(keymap-global-set "C-<left>" #'paredit-forward-barf-sexp)
(keymap-global-set "C-<right>" #'paredit-forward-slurp-sexp)
(keymap-global-set "C-M-<left>" #'paredit-backward-slurp-sexp)
(keymap-global-set "C-M-<right>" #'paredit-backward-barf-sexp)
(keymap-set evil-normal-state-map "M-r" #'raise-sexp)
(keymap-set evil-normal-state-map "M-s" #'paredit-splice-sexp)

(defun init-wrap-pair (&optional arg)
  "Insert pair, ARG see `insert-pair'."
  (interactive "*P")
  (insert-pair (or arg 1))
  (indent-sexp))

;;; input method

(keymap-global-set "C-SPC" #'toggle-input-method)
(keymap-global-set "C-@" #'toggle-input-method)

(dolist (state '(operator motion normal visual))
  (setf (plist-get (cdr (assq state evil-state-properties)) :input-method) t))

;; ugly work around before https://github.com/emacs-evil/evil/pull/1995 merged

(defvar init-ignore-toggle-input-method nil)

(defun toggle-input-method@check-ignore (&rest _)
  "Check ignore before toggle input method."
  init-ignore-toggle-input-method)

(defun evil-state@ignore-toggle-input-method (func &rest args)
  "Ignore toggle input method around command.
FUNC ARGS see specified commands."
  (let ((init-ignore-toggle-input-method t))
    (apply func args)))

(dolist (func '(toggle-input-method
                activate-input-method
                deactivate-input-method))
  (advice-add func :before-until #'toggle-input-method@check-ignore))

(dolist (func '(evil-local-mode
                evil-emacs-state
                evil-insert-state
                evil-replace-state
                evil-operator-state
                evil-motion-state
                evil-normal-state
                evil-visual-state))
  (advice-add func :around #'evil-state@ignore-toggle-input-method))

(defun init-ignore-input-method-p ()
  "Predicate of input method."
  (and evil-local-mode
       (memq evil-state '(operator motion normal visual))))

(defun init-wrap-input-method (func event)
  "Wrap a `input-method-function' FUNC that process ignore and jk escape.
FUNC, EVENT see `input-method-function'."
  (if (init-ignore-input-method-p)
      (list event)
    (if (or (/= event ?j) (sit-for 0.15))
        (funcall func event)
      (let ((next-event (read-event)))
        (if (/= next-event ?k)
            (progn
              (push next-event unread-command-events)
              (funcall func event))
          (push 'escape unread-command-events)
          nil)))))

(defun init-input-method (event)
  "Default input method function.
EVENT see `input-method-function'."
  (init-wrap-input-method #'list event))

(setq-default input-method-function #'init-input-method)

;;; minibuffer

(require 'savehist)
(require 'orderless)
(require 'marginalia)

(setq enable-recursive-minibuffers t)
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides nil)
(setq read-extended-command-predicate #'command-completion-default-include-p)

(keymap-set minibuffer-local-map "<remap> <quit-window>" #'abort-recursive-edit)

(init-evil-keymap-set 'normal minibuffer-local-map
  "<escape>" #'abort-recursive-edit)

(savehist-mode 1)
(marginalia-mode 1)

;;;; vertico

(defvar vertico-mode-map (make-sparse-keymap))

(require 'vertico)
(require 'vertico-multiform)
(require 'vertico-repeat)
(require 'vertico-suspend)
(require 'vertico-directory)

(setq vertico-resize nil)

(setq vertico-multiform-categories
      '((file (:keymap . vertico-directory-map))))

(vertico-mode 1)
(vertico-multiform-mode 1)

(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

(keymap-set vertico-mode-map "C-c b" #'vertico-repeat)
(keymap-set vertico-mode-map "C-c z" #'vertico-suspend)

(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(keymap-unset vertico-directory-map "RET" t)
(keymap-set vertico-directory-map "C-l" #'vertico-directory-up)

(keymap-set vertico-map "C-x C-s" #'embark-export)

(keymap-set vertico-map "<remap> <evil-scroll-down>" #'vertico-scroll-up)
(keymap-set vertico-map "<remap> <evil-scroll-up>" #'vertico-scroll-down)
(keymap-set vertico-map "<remap> <evil-next-line>" #'vertico-next)
(keymap-set vertico-map "<remap> <evil-previous-line>" #'vertico-previous)
(keymap-set vertico-map "<remap> <evil-next-visual-line>" #'vertico-next)
(keymap-set vertico-map "<remap> <evil-previous-visual-line>" #'vertico-previous)
(keymap-set vertico-map "<remap> <evil-goto-first-line>" #'vertico-first)
(keymap-set vertico-map "<remap> <evil-goto-line>" #'vertico-last)

;;; search

(require 'consult)
(require 'consult-imenu)
(require 'embark-consult)

(setq consult-preview-key '(:debounce 0.2 any))

(setq completion-in-region-function #'consult-completion-in-region)

(keymap-set evil-insert-state-map "M-r" #'consult-history)

(defvar-keymap init-consult-override-mode-map)

(define-minor-mode init-consult-override-mode
  "Override consult commands."
  :group 'init-consult
  :global t
  :init-value t
  :keymap init-consult-override-mode-map)

(keymap-set init-consult-override-mode-map "<remap> <yank>" #'consult-yank-from-kill-ring)
(keymap-set init-consult-override-mode-map "<remap> <yank-pop>" #'consult-yank-pop)
(keymap-set init-consult-override-mode-map "<remap> <goto-line>" #'consult-goto-line)

(setq consult-line-start-from-top t)

(define-advice consult-line (:after (&rest _) set-history)
  (let ((pattern (car consult--line-history)))
    (setq evil-ex-search-pattern (list pattern t t))
    (evil-ex-search-activate-highlight evil-ex-search-pattern)))

(defun init-consult-search (&optional arg initial)
  "Consult search with INITIAL.
Without universal ARG, search in this buffer with `consult-line'.
With one universal ARG, search in project directory with `consult-ripgrep'.
With two universal ARG, prompt for directory to search with `consult-ripgrep'."
  (interactive "P")
  (cond ((> (prefix-numeric-value arg) 4)
         (setq this-command #'consult-ripgrep)
         (consult-ripgrep t initial))
        (arg
         (setq this-command #'consult-ripgrep)
         (consult-ripgrep nil initial))
        (t
         (setq this-command #'consult-line)
         (consult-line initial))))

(defun init-consult-search-dwim (&optional arg)
  "Consult search dwim.
ARG see `init-consult-search'."
  (interactive "P")
  (init-consult-search arg (thing-at-point 'symbol)))

(keymap-set search-map "s" #'init-consult-search)
(keymap-global-set "C-s" #'init-consult-search-dwim)

;;; outline

(require 'outline)

(setq outline-minor-mode-cycle t)
(setq outline-minor-mode-highlight 'override)
(setq outline-minor-mode-use-buttons 'in-margins)

(defun init-outline-narrow-to-subtree ()
  "Narrow to outline subtree."
  (interactive)
  (save-excursion
    (save-match-data
      (narrow-to-region
       (progn (outline-back-to-heading t) (point))
       (progn (outline-end-of-subtree)
              (when (and (outline-on-heading-p) (not (eobp)))
                (backward-char 1))
              (point))))))

(keymap-set narrow-map "s" #'init-outline-narrow-to-subtree)

;;; occur

(keymap-set occur-mode-map "C-c C-e" #'occur-edit-mode)
(evil-set-initial-state 'occur-edit-mode 'normal)

;;; dired

(require 'dired)
(require 'wdired)

(setq dired-dwim-target t)
(setq dired-auto-revert-buffer t)
(setq dired-listing-switches "-lha")

(put 'dired-jump 'repeat-map nil)

(keymap-set ctl-x-4-map "j" #'dired-jump-other-window)
(keymap-set project-prefix-map "j" #'project-dired)

(keymap-set dired-mode-map "C-c C-e" #'wdired-change-to-wdired-mode)

(init-evil-keymap-set 'normal dired-mode-map
  "j" #'dired-next-line
  "k" #'dired-previous-line
  "o" #'dired-find-file-other-window
  "m" #'dired-mark
  "u" #'dired-unmark
  "U" #'dired-unmark-all-marks
  "t" #'dired-toggle-marks
  "d" #'dired-flag-file-deletion
  "x" #'dired-do-flagged-delete
  "s" #'dired-sort-toggle-or-edit
  "i" #'dired-insert-subdir
  "D" #'dired-do-delete
  "C" #'dired-do-copy
  "R" #'dired-do-rename
  "Z" #'dired-do-compress
  "+" #'dired-create-directory
  "=" #'dired-diff
  "!" #'dired-do-shell-command
  "&" #'dired-do-async-shell-command)

(require 'arc-mode)

(init-evil-keymap-set 'normal archive-mode-map
  "j" #'archive-next-line
  "k" #'archive-previous-line
  "o" #'archive-extract-other-window
  "m" #'archive-mark
  "u" #'archive-unflag
  "C" #'archive-copy-file)

;;; image

(require 'image-mode)

(keymap-set image-mode-map "<remap> <evil-next-line>" #'image-next-line)
(keymap-set image-mode-map "<remap> <evil-previous-line>" #'image-previous-line)
(keymap-set image-mode-map "<remap> <evil-next-visual-line>" #'image-next-line)
(keymap-set image-mode-map "<remap> <evil-previous-visual-line>" #'image-previous-line)
(keymap-set image-mode-map "<remap> <evil-backward-char>" #'image-backward-hscroll)
(keymap-set image-mode-map "<remap> <evil-forward-char>" #'image-forward-hscroll)
(keymap-set image-mode-map "<remap> <evil-scroll-down>" #'image-scroll-up)
(keymap-set image-mode-map "<remap> <evil-scroll-up>" #'image-scroll-down)
(keymap-set image-mode-map "<remap> <evil-scroll-left>" #'image-scroll-right)
(keymap-set image-mode-map "<remap> <evil-scroll-right>" #'image-scroll-left)
(keymap-set image-mode-map "<remap> <evil-goto-first-line>" #'image-bob)
(keymap-set image-mode-map "<remap> <evil-goto-line>" #'image-eob)

(keymap-set image-mode-map "C-=" #'image-increase-size)
(keymap-set image-mode-map "C-+" #'image-increase-size)
(keymap-set image-mode-map "C--" #'image-decrease-size)

(keymap-set image-mode-map "M-n" #'image-next-file)
(keymap-set image-mode-map "M-p" #'image-previous-file)

(init-evil-keymap-set 'normal image-mode-map
  "m" #'image-mode-mark-file
  "u" #'image-mode-unmark-file)

;;; process

;;;; compile

(require 'compile)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(keymap-set project-prefix-map "C" #'project-recompile)

;;;; grep

(require 'grep)
(require 'wgrep)

(setq wgrep-auto-save-buffer t)
(setq wgrep-change-readonly-file t)

(defvar init-rg-program "rg")

(defun init-rg-dwim (&optional arg)
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
         (command-default (format "%s -n --no-heading --color=always -S %s ." init-rg-program pattern))
         (command (if (> (prefix-numeric-value arg) 4)
                      (read-string "Search command: " command-default 'grep-history)
                    command-default)))
    (grep--save-buffers)
    (compilation-start command 'grep-mode)))

(defalias 'rg 'init-rg-dwim)

;;;; comint

(require 'comint)

(defun init-comint-set-outline ()
  "Set outline vars for comint."
  (setq-local outline-regexp comint-prompt-regexp)
  (setq-local outline-level (lambda () 1)))

(add-hook 'comint-mode-hook #'init-comint-set-outline)

;;;; eshell

(require 'eshell)
(require 'em-prompt)
(require 'em-hist)
(require 'em-cmpl)
(require 'em-dirs)
(require 'em-alias)

(setq eshell-aliases-file (expand-file-name "eshell-alias.esh" priv-directory))

(defun init-eshell-set-outline ()
  "Set outline vars for Eshell."
  (setq-local outline-regexp "^[^#$\n]* [#$] ")
  (setq-local outline-level (lambda () 1)))

(add-hook 'eshell-mode-hook #'init-eshell-set-outline)

(keymap-unset eshell-cmpl-mode-map "C-M-i" t)

(defun init-eshell-dwim-find-buffer ()
  "Find eshell dwim buffer."
  (seq-find
   (lambda (buffer)
     (and (eq (buffer-local-value 'major-mode buffer) 'eshell-mode)
          (string-prefix-p eshell-buffer-name (buffer-name buffer))
          (not (get-buffer-process buffer))
          (not (get-buffer-window buffer))))
   (buffer-list)))

(defun init-eshell-dwim-get-buffer-create ()
  "Get eshell dwim buffer, create if not exist."
  (if-let* ((buffer (init-eshell-dwim-find-buffer)))
      (let ((dir default-directory))
        (with-current-buffer buffer
          (eshell/cd dir)
          (eshell-reset)
          (current-buffer)))
    (with-current-buffer (generate-new-buffer eshell-buffer-name)
      (eshell-mode)
      (current-buffer))))

(defun init-eshell-dwim (&optional arg)
  "Do open eshell smartly.
Without universal ARG, open in other window.
With universal ARG, open in this window."
  (interactive "P")
  (let ((buffer (init-eshell-dwim-get-buffer-create)))
    (if arg
        (switch-to-buffer buffer)
      (switch-to-buffer-other-window buffer))))

;;; vc

;;;; ediff

(require 'ediff)

(setq ediff-window-setup-function #'ediff-setup-windows-plain)

(evil-set-initial-state 'ediff-mode 'emacs)

(defun init-ediff-scroll-up ()
  "Scroll up in ediff."
  (interactive)
  (let ((last-command-event ?V))
    (call-interactively #'ediff-scroll-vertically)))

(defun init-ediff-scroll-down ()
  "Scroll down in ediff."
  (interactive)
  (let ((last-command-event ?v))
    (call-interactively #'ediff-scroll-vertically)))

(defun init-ediff-scroll-left ()
  "Scroll left in ediff."
  (interactive)
  (let ((last-command-event ?>))
    (call-interactively #'ediff-scroll-horizontally)))

(defun init-ediff-scroll-right ()
  "Scroll right in ediff."
  (interactive)
  (let ((last-command-event ?<))
    (call-interactively #'ediff-scroll-horizontally)))

(defun init-ediff-jump-to-last-difference ()
  "Jump to last difference."
  (interactive)
  (ediff-jump-to-difference -1))

(define-advice ediff-setup-keymap (:after () evil)
  (keymap-set ediff-mode-map "j" #'ediff-next-difference)
  (keymap-set ediff-mode-map "k" #'ediff-previous-difference)
  (keymap-set ediff-mode-map "g g" #'ediff-jump-to-difference)
  (keymap-set ediff-mode-map "G" #'init-ediff-jump-to-last-difference)
  (keymap-set ediff-mode-map "C-d" #'init-ediff-scroll-down)
  (keymap-set ediff-mode-map "C-u" #'init-ediff-scroll-up)
  (keymap-set ediff-mode-map "<up>" #'init-ediff-scroll-up)
  (keymap-set ediff-mode-map "<down>" #'init-ediff-scroll-down)
  (keymap-set ediff-mode-map "<left>" #'init-ediff-scroll-left)
  (keymap-set ediff-mode-map "<right>" #'init-ediff-scroll-right))

;;;; with editor

(require 'with-editor)

(shell-command-with-editor-mode 1)

(add-hook 'shell-mode-hook #'with-editor-export-editor)
(add-hook 'eshell-mode-hook #'with-editor-export-editor)

;;;; magit

(require 'magit)

(keymap-set project-prefix-map "v" #'magit-project-status)

(keymap-set magit-mode-map "<remap> <quit-window>" #'magit-mode-bury-buffer)

(evil-set-initial-state 'magit-mode 'normal)

(init-evil-keymap-set 'motion magit-mode-map
  "," #'magit-dispatch)

(defvar init-magit-normal-command-keys
  (list
   ?a ?A ?b ?B ?c ?C ?d ?D ?e ?E ?f ?F ?i ?I ?m ?M
   ?o ?O ?r ?R ?s ?S ?t ?T ?u ?U ?w ?W ?x ?X ?z ?Z))

(apply
 #'evil-define-key* 'normal magit-mode-map
 "p" #'magit-push
 "P" #'magit-push
 (seq-mapcat
  (lambda (key)
    (list (vector key) (lookup-key magit-mode-map (vector key))))
  init-magit-normal-command-keys))

(keymap-set magit-blob-mode-map "<remap> <quit-window>" #'magit-kill-this-buffer)
(keymap-set magit-blob-mode-map "M-n" #'magit-blob-next)
(keymap-set magit-blob-mode-map "M-p" #'magit-blob-previous)

(keymap-set magit-blame-mode-map "<remap> <quit-window>" #'magit-blame-quit)
(keymap-set magit-blame-mode-map "M-n" #'magit-blame-next-chunk)
(keymap-set magit-blame-mode-map "M-p" #'magit-blame-previous-chunk)

;;; prog

;;;; abbrev

(setq-default abbrev-mode t)

(defvar yas-alias-to-yas/prefix-p)
(setq yas-alias-to-yas/prefix-p nil)

(require 'yasnippet)

(setcdr (assq 'yas-minor-mode minor-mode-alist) '(" Yas"))

(keymap-unset yas-minor-mode-map "TAB" t)
(keymap-set yas-keymap "TAB" #'yas-next-field)

(add-hook 'after-init-hook #'yas-global-mode)

(defun init-abbrev-yas-define (table abbrev snippet &optional env)
  "Define an ABBREV in TABLE, to expand a yas SNIPPET with ENV."
  (let ((length (length abbrev))
        (hook (make-symbol abbrev))
        (ensure-pair (car (alist-get 'ensure-pair env))))
    (put hook 'no-self-insert t)
    (fset hook (lambda ()
                 (delete-char (- length))
                 (when (and ensure-pair (/= ?\( (char-before)))
                   (insert-pair 0 ?\( ?\)))
                 (yas-expand-snippet snippet nil nil env)))
    (define-abbrev table abbrev 'yas hook :system t)))

(defun init-abbrev-define (table abbrev expansion)
  "Define an ABBREV in TABLE, to expand as EXPANSION.
EXPANSION may be:
- text: (text \"expansion\")
- yas: (yas \"expansion\" (ENVSYM ENVVAL) ...)"
  (let ((expansion-type (car expansion))
        (expansion (cdr expansion)))
    (cond ((eq expansion-type 'text)
           (define-abbrev table abbrev (car expansion) nil :system t))
          ((eq expansion-type 'yas)
           (init-abbrev-yas-define table abbrev (car expansion) (cdr expansion)))
          (t
           (user-error "Invalid abbrev expansion type")))))

(defun init-abbrev-define-table (tablename defs)
  "Define abbrev table with TABLENAME and abbrevs DEFS."
  (let ((table (if (boundp tablename) (symbol-value tablename))))
    (unless table
      (setq table (make-abbrev-table))
      (set tablename table))
    (unless (memq tablename abbrev-table-name-list)
      (push tablename abbrev-table-name-list))
    (dolist (def defs)
      (init-abbrev-define table (car def) (cdr def)))))

(defvar init-abbrev-file
  (expand-file-name "abbrevs.eld" priv-directory))

(defun init-abbrev-load (&optional file)
  "Load abbrevs FILE."
  (interactive)
  (let ((file (or file init-abbrev-file)))
    (when (file-exists-p file)
      (let ((defs (with-temp-buffer
                    (insert-file-contents file)
                    (read (buffer-string)))))
        (dolist (def defs)
          (init-abbrev-define-table (car def) (cdr def)))))))

(add-hook 'after-init-hook #'init-abbrev-load)

;;;; company

(require 'company)
(require 'company-files)
(require 'company-capf)
(require 'company-keywords)
(require 'company-dabbrev)
(require 'company-dabbrev-code)

(global-company-mode 1)

(setq company-lighter-base "Company")
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)
(setq company-show-quick-access t)
(setq company-tooltip-align-annotations t)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case t)
(setq company-dabbrev-code-ignore-case t)
(setq company-frontends '(company-pseudo-tooltip-frontend company-preview-if-just-one-frontend company-echo-metadata-frontend))
(setq company-backends '(company-files company-capf (company-dabbrev-code company-keywords) company-dabbrev))

(keymap-unset company-active-map "M-n" t)
(keymap-unset company-active-map "M-p" t)
(keymap-set company-mode-map "C-c c" #'company-complete)

(defvar init-minibuffer-company-backends '(company-capf))
(defvar init-minibuffer-company-frontends '(company-pseudo-tooltip-frontend company-preview-if-just-one-frontend))

(defun init-minibuffer-set-company ()
  "Set company in minibuffer."
  (setq-local company-backends init-minibuffer-company-backends)
  (setq-local company-frontends init-minibuffer-company-frontends)
  (when global-company-mode
    (company-mode 1)))

(add-hook 'minibuffer-mode-hook #'init-minibuffer-set-company)

(define-advice company-capf (:around (func &rest args) set-styles)
  (let ((completion-styles '(basic partial-completion)))
    (apply func args)))

(define-advice company-call-backend (:before-until (command &rest _) check-evil)
  (and (eq command 'prefix)
       evil-local-mode
       (not (memq evil-state '(insert replace emacs)))))

;;;; eglot

(require 'flymake)
(require 'flymake-proc)
(setq flymake-no-changes-timeout 1)
(setq flymake-show-diagnostics-at-end-of-line 'short)
(remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)
(keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
(keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error)

(require 'eldoc)
(setq eldoc-minor-mode-string nil)
(keymap-set prog-mode-map "<remap> <display-local-help>" #'eldoc-doc-buffer)

(require 'xref)
(setq xref-search-program 'ripgrep)

(require 'eglot)
(setq eglot-extend-to-xref t)
(keymap-set eglot-mode-map "<remap> <evil-lookup>" #'eldoc-doc-buffer)

;;; elisp

;;;; lisp

(defun init-lisp-outline-level ()
  "Return level of current outline heading."
  (when (looking-at ";;\\([;*]+\\)")
    (- (match-end 1) (match-beginning 1))))

(defun init-lisp-set-outline ()
  "Set outline vars."
  (setq-local outline-regexp ";;[;*]+[\s\t]+")
  (setq-local outline-level #'init-lisp-outline-level)
  (outline-minor-mode 1))

(defun init-wrap-next-sexp-command (func)
  "Goto sexp end, then call last-sexp command FUNC."
  (save-excursion
    (when-let* ((end (cdr (bounds-of-thing-at-point 'sexp))))
      (goto-char end))
    (call-interactively func)))

(defmacro init-define-next-sexp-command (last-sexp-command)
  "Remap LAST-SEXP-COMMAND to next-sexp-command in evil motion state."
  (let ((next-sexp-command (intern (concat "init-next-sexp@" (symbol-name last-sexp-command)))))
    `(prog1
         (defun ,next-sexp-command ()
           (interactive)
           (init-wrap-next-sexp-command ',last-sexp-command))
       (define-key evil-motion-state-map [remap ,last-sexp-command] ',next-sexp-command))))

;;;; elisp

(init-define-next-sexp-command eval-last-sexp)
(init-define-next-sexp-command eval-print-last-sexp)
(init-define-next-sexp-command pp-eval-last-sexp)
(init-define-next-sexp-command pp-macroexpand-last-sexp)

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (keymap-set map "C-c C-k" #'eval-buffer)
  (keymap-set map "C-C C-l" #'load-file)
  (keymap-set map "C-c C-m" #'pp-macroexpand-last-sexp))

(add-hook 'emacs-lisp-mode-hook #'init-lisp-set-outline)

(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (add-to-list 'init-evil-eval-function-alist `(,mode . eval-region)))

(require 'ielm)

(defun init-ielm-other-window ()
  "Switch to elisp repl other window."
  (interactive)
  (pop-to-buffer (get-buffer-create "*ielm*"))
  (ielm))

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (keymap-set map "C-c C-z" #'init-ielm-other-window))

;;;; help

(keymap-set help-map "B" #'describe-keymap)
(keymap-set help-map "p" #'describe-package)

(keymap-set help-map "L" #'find-library)
(keymap-set help-map "F" #'find-function)
(keymap-set help-map "V" #'find-variable)
(keymap-set help-map "K" #'find-function-on-key)
(keymap-set help-map "4 L" #'find-library-other-window)
(keymap-set help-map "4 F" #'find-function-other-window)
(keymap-set help-map "4 V" #'find-variable-other-window)
(keymap-set help-map "4 K" #'find-function-on-key-other-window)
(keymap-set help-map "5 L" #'find-library-other-frame)
(keymap-set help-map "5 F" #'find-function-other-frame)
(keymap-set help-map "5 V" #'find-variable-other-frame)
(keymap-set help-map "5 K" #'find-function-on-key-other-frame)

(defvar-keymap init-load-map
  "f" #'load-file
  "l" #'load-library
  "t" #'load-theme)

(keymap-set help-map "t" init-load-map)

(consult-customize consult-theme :preview-key '(:debounce 0.5 any))
(keymap-set init-consult-override-mode-map "<remap> <load-theme>" #'consult-theme)

(defun init-describe-symbol-dwim ()
  "Describe symbol at point."
  (interactive)
  (describe-symbol (symbol-at-point)))

(setq evil-lookup-func #'init-describe-symbol-dwim)

;;; org

(require 'org)
(require 'org-macs)
(require 'org-agenda)
(require 'org-capture)
(require 'consult-org)
(require 'embark-org)
(require 'orglink)

(add-to-list 'org-modules 'org-id)
(add-to-list 'org-modules 'org-mouse)
(add-to-list 'org-modules 'org-tempo)
(add-to-list 'org-modules 'ol-eshell)

(setq org-special-ctrl-a/e t)
(setq org-sort-function #'org-sort-function-fallback)
(setq org-tags-sort-function #'org-string<)

(setq org-directory (expand-file-name "org" user-emacs-directory))
(setq org-agenda-files (list org-directory))
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

(setq org-capture-templates
      '(("t" "Todo"                      entry (file "") "* TODO %?\n%U")
        ("a" "Todo With Annotation"      entry (file "") "* TODO %?\n%U\n%a")
        ("i" "Todo With Initial Content" entry (file "") "* TODO %?\n%U\n%i")
        ("c" "Todo With Kill Ring"       entry (file "") "* TODO %?\n%U\n%c")))

(defun init-org-set-syntax ()
  "Modify `org-mode' syntax table."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(add-hook 'org-mode-hook #'init-org-set-syntax)

(defun init-org-append-link ()
  "Append org link."
  (interactive)
  (save-excursion
    (unless (eolp)
      (forward-char))
    (call-interactively #'org-insert-link)))

(defun init-org-append-link-global ()
  "Append org link outside org."
  (interactive)
  (save-excursion
    (unless (eolp)
      (forward-char))
    (call-interactively #'org-insert-link-global)))

(defun init-org-echo-link ()
  "Echo org link in minibuffer."
  (interactive)
  (when (org-in-regexp org-link-any-re)
    (let (message-log-max)
      (message "%s" (match-string-no-properties 0)))))

(keymap-global-set "C-c o" #'org-open-at-point-global)
(keymap-global-set "C-c l" #'org-insert-link-global)

(keymap-set org-mode-map "<remap> <org-open-at-point-global>" #'org-open-at-point)
(keymap-set org-mode-map "<remap> <org-insert-link-global>" #'org-insert-link)

(keymap-set evil-normal-state-map "<remap> <org-insert-link>" #'init-org-append-link)
(keymap-set evil-normal-state-map "<remap> <org-insert-link-global>" #'init-org-append-link-global)

(keymap-set embark-org-link-map "e" #'init-org-echo-link)

(define-advice orglink-mode--turn-on (:override () override)
  (when (and (derived-mode-p 'text-mode 'prog-mode)
             (not (derived-mode-p 'org-mode)))
    (orglink-mode 1)))

(global-orglink-mode 1)

(defun init-text-set-orglink-anywhere ()
  "Set orglink for text modes."
  (interactive)
  (setq-local orglink-match-anywhere t))

(add-hook 'text-mode-hook #'init-text-set-orglink-anywhere)

(keymap-set org-mode-map "<remap> <consult-imenu>" #'consult-org-heading)

(keymap-set org-mode-map "C-c C-'" #'org-edit-special)
(keymap-set org-src-mode-map "C-c C-'" #'org-edit-src-exit)
(keymap-set org-src-mode-map "C-c C-c" #'org-edit-src-exit)

;;; markdown

(require 'markdown-mode)
(require 'edit-indirect)

(setq markdown-special-ctrl-a/e t)
(setq markdown-fontify-code-blocks-natively t)

(add-hook 'markdown-mode-hook #'outline-minor-mode)

(keymap-set markdown-mode-map "C-c C-'" #'markdown-edit-code-block)
(keymap-set edit-indirect-mode-map "C-c C-'" #'edit-indirect-commit)

;;; leaders

(defvar-keymap init-evil-override-mode-map)

(define-minor-mode init-evil-override-mode
  "Override leader prefix map."
  :group 'init-evil
  :global t
  :init-value t
  :keymap init-evil-override-mode-map)

(defvar-keymap init-leader-map)

(defun init-leader-set (&rest clauses)
  "Set leader binding CLAUSES in `init-leader-map'."
  (dolist (binding (seq-partition clauses 2))
    (keymap-set init-leader-map (car binding) (cadr binding))))

(defun init-leader-wrap-spc (command)
  "Wrap COMMAND on spc as leader key."
  (if (eq last-command-event 32)
      (set-transient-map init-leader-map)
    (setq this-command command)
    (setq real-this-command command)
    (call-interactively command)))

(defun init-leader-or-scroll-up-command ()
  "Leader aware scroll up command."
  (interactive)
  (init-leader-wrap-spc #'scroll-up-command))

(init-evil-keymap-set 'motion init-evil-override-mode-map
  "SPC" init-leader-map
  "<remap> <scroll-up-command>" #'init-leader-or-scroll-up-command)

(defun init-magic-prefix (prefix)
  "Magically read and execute command on PREFIX."
  (let ((char (read-char (concat prefix " C-"))))
    (if (= char ?\C-h)
        (describe-keymap (key-binding (kbd prefix)))
      (let ((literal (= char ? ))
            new-prefix binding)
        (when literal
          (setq char (read-char prefix)))
        (unless literal
          (setq new-prefix (concat prefix " C-" (char-to-string char))
                binding (key-binding (kbd new-prefix))))
        (unless binding
          (setq new-prefix (concat prefix " " (char-to-string char))
                binding (key-binding (kbd new-prefix))))
        (cond ((and binding (commandp binding))
               (setq this-command binding)
               (setq real-this-command binding)
               (if (commandp binding t)
                   (call-interactively binding)
                 (execute-kbd-macro binding)))
              ((and binding (keymapp binding))
               (init-magic-prefix new-prefix))
              (t
               (user-error "No magic key binding found on %s %c" prefix char)))))))

(defun init-magic-C-c ()
  "Magic control C."
  (interactive)
  (init-magic-prefix "C-c"))

(defun init-magic-C-u ()
  "Magic control U."
  (interactive)
  (setq prefix-arg
        (list (if current-prefix-arg
                  (* 4 (prefix-numeric-value current-prefix-arg))
                4)))
  (set-transient-map init-leader-map))

(defvar init-magic-shift-special
  '((?1 . ?!) (?2 . ?@) (?3 . ?#) (?4 . ?$) (?5 . ?%) (?6 . ?^) (?7 . ?&) (?8 . ?*) (?9 . ?\() (?0 . ?\))
    (?- . ?_) (?= . ?+) (?` . ?~) (?\[ . ?\{) (?\] . ?\}) (?\\ . ?|) (?, . ?<) (?. . ?>) (?/ . ??)))

(defun init-magic-shift ()
  "Magic shift."
  (interactive)
  (let* ((char (read-char "<leader>-"))
         (shift-char (or (cdr (assq char init-magic-shift-special)) (upcase char))))
    (if-let* ((binding (lookup-key init-leader-map (vector shift-char))))
        (if (commandp binding)
            (progn
              (setq this-command binding)
              (setq real-this-command binding)
              (if (commandp binding t)
                  (call-interactively binding)
                (execute-kbd-macro binding)))
          (user-error "Binding on <leader> %c is not a command" shift-char))
      (user-error "No binding found on <leader> %c" shift-char))))

(defvar-keymap init-minor-prefix-map
  "s" #'auto-save-visited-mode
  "r" #'global-auto-revert-mode
  "t" #'toggle-truncate-lines
  "S" #'init-toggle-scroll-bar
  "v" #'visual-line-mode
  "w" #'whitespace-mode
  "W" #'whitespace-newline-mode
  "h" #'hl-line-mode
  "n" #'display-line-numbers-mode
  "N" #'init-toggle-line-numbers-relative
  "d" #'eldoc-doc-buffer
  "e" #'flymake-show-buffer-diagnostics
  "E" #'flymake-show-project-diagnostics)

(init-leader-set
 "SPC" #'consult-buffer
 "\\" #'init-magic-shift
 "TAB" #'init-magic-shift
 "c" #'init-magic-C-c
 "u" #'init-magic-C-u
 "z" #'repeat
 ";" #'eval-expression
 "!" #'shell-command
 "&" #'async-shell-command
 "0" #'delete-window
 "1" #'delete-other-windows
 "2" #'split-window-below
 "3" #'split-window-right
 "o" #'other-window
 "q" #'quit-window
 "b" #'switch-to-buffer
 "k" #'init-kill-current-buffer
 "f" #'find-file
 "d" #'dired
 "j" #'dired-jump
 "S" #'init-rg-dwim
 "e" #'init-eshell-dwim
 "A" #'org-agenda
 "C" #'org-capture
 "W" #'org-store-link
 "O" #'org-open-at-point-global
 "L" #'org-insert-link-global
 "T" #'consult-org-agenda
 "w" evil-window-map
 "4" ctl-x-4-map
 "5" ctl-x-5-map
 "t" tab-prefix-map
 "p" project-prefix-map
 ;; "v" vc-prefix-map
 "v" #'magit-file-dispatch
 "V" #'magit-dispatch
 "x" ctl-x-x-map
 "r" ctl-x-r-map
 "h" help-map
 "g" goto-map
 "s" search-map
 "n" narrow-map
 "a" abbrev-map
 "m" init-minor-prefix-map
 "$" #'ispell-word
 "%" #'query-replace-regexp
 "=" #'init-indent-dwim
 "+" #'delete-trailing-whitespace
 "." #'xref-find-definitions
 "?" #'xref-find-references
 "," #'xref-go-back
 "i" #'consult-imenu
 "l" #'consult-outline
 "(" #'init-wrap-pair
 "[" #'init-wrap-pair
 "{" #'init-wrap-pair
 "<" #'init-wrap-pair
 "'" #'init-wrap-pair
 "`" #'init-wrap-pair
 "\"" #'init-wrap-pair)

;;; end

(provide 'init-emacs)
;;; init-emacs.el ends here
