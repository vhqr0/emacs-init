;;; init-emacs.el --- Init Emacs -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Emacs itself.

(require 'init-core)

;;; Code:

;;; utils

(defun init-diminish-minor-mode (mode)
  "Diminish MODE lighter."
  (setq minor-mode-alist
        (seq-remove
         (lambda (alist) (eq (car alist) mode))
         minor-mode-alist)))

(defun init-filtered-command (filter command)
  "Make filtered command.
FILTER COMMAND see menu item."
  `(menu-item "" ,command :filter ,filter))

(defun init-region-bounds ()
  "Get region bounds."
  (when (use-region-p)
    (cons (region-beginning) (region-end))))

(defun init-buffer-bounds ()
  "Get buffer bounds."
  (cons (point-min) (point-max)))

(defun init-region-or-buffer-bounds ()
  "Get region bounds or buffer bounds."
  (or (init-region-bounds) (init-buffer-bounds)))

(defun init-region-content ()
  "Get region content or nil."
  (when-let* ((bounds (init-region-bounds)))
    (buffer-substring (car bounds) (cdr bounds))))

(defun init-thing-at-point ()
  "Get thing at point dwim."
  (or (init-region-content) (thing-at-point 'symbol)))

(defun init-thing-at-point-or-throw ()
  "Get thing at point dwim, or throw user error."
  (or (init-thing-at-point) (user-error "No thing at point")))

(defun init-project-directory ()
  "Get current project directory."
  (when-let* ((project (project-current)))
    (project-root project)))

(defun init-directory ()
  "Get current project directory or default directory."
  (or (init-project-directory) default-directory))

(defun init-directory-interactive (arg prompt)
  "Get directory smartly.
With universal ARG read directory with PROMPT."
  (if arg
      (read-directory-name prompt)
    (init-directory)))

(defun init-switch-to-buffer-split-window (buffer)
  "Switch to BUFFER split at this window."
  (let ((parent (window-parent (selected-window))))
    (cond ((window-left-child parent)
           (select-window (split-window-vertically))
           (switch-to-buffer buffer))
          ((window-top-child parent)
           (select-window (split-window-horizontally))
           (switch-to-buffer buffer))
          (t
           (switch-to-buffer-other-window buffer)))))

(defun init-switch-to-buffer-split-window-interactive (arg buffer)
  "Do switch to BUFFER in split window smartly, with interactive ARG.
Without universal ARG, open in split window.
With one universal ARG, open other window.
With two or more universal ARG, open in current window."
  (cond ((> (prefix-numeric-value arg) 4)
         (switch-to-buffer buffer))
        (arg
         (switch-to-buffer-other-window buffer))
        (t
         (init-switch-to-buffer-split-window buffer))))

(defun init-read-file-contents (file)
  "Read contents of text FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties (point-min) (point-max))))

;;; initial

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

(defvar init-disable-ui-modes
  '(blink-cursor-mode tool-bar-mode menu-bar-mode))

(defun init-disable-ui ()
  "Disable various ui modes."
  (interactive)
  (dolist (mode init-disable-ui-modes)
    (when (fboundp mode)
      (funcall mode -1))))

(add-hook 'after-init-hook #'init-disable-ui)

;;; evil

(defvar evil-want-keybinding)
(setq evil-want-keybinding nil)

(defvar evil-want-minibuffer)
(setq evil-want-minibuffer t)

(defvar evil-want-C-i-jump)
(setq evil-want-C-i-jump nil)

(defvar evil-want-C-u-scroll)
(defvar evil-want-C-w-delete)
(defvar evil-want-Y-yank-to-eol)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-w-delete t)
(setq evil-want-Y-yank-to-eol t)

(defvar evil-respect-visual-line-mode)
(setq evil-respect-visual-line-mode t)

(defvar evil-undo-system)
(setq evil-undo-system 'undo-redo)

(require 'evil)

(setq evil-mode-line-format '(before . mode-line-front-space))

(setq evil-want-fine-undo t)
(setq evil-symbol-word-search t)

(setq evil-goto-definition-functions
      '(evil-goto-definition-imenu evil-goto-definition-xref))

(setq evil-emacs-state-modes nil)
(setq evil-insert-state-modes nil)
(setq evil-motion-state-modes '(special-mode))

(add-hook 'after-init-hook #'evil-mode)

(defvar init-evil-disable-adjust-cursor-commands
  '(forward-sexp forward-list))

(defun init-evil-around-adjust-cursor-filter-commands (func &rest args)
  "Dont adjust cursor after certain commands.
FUNC and ARGS see `evil-set-cursor'."
  (unless (memq this-command init-evil-disable-adjust-cursor-commands)
    (apply func args)))

(advice-add #'evil-adjust-cursor :around #'init-evil-around-adjust-cursor-filter-commands)

(keymap-unset evil-normal-state-map "<remap> <yank-pop>" t)

(keymap-unset evil-insert-state-map "C-@" t)
(keymap-unset evil-insert-state-map "C-a" t)
(keymap-unset evil-insert-state-map "C-k" t)
(keymap-unset evil-insert-state-map "C-w" t)
(keymap-unset evil-insert-state-map "C-e" t)
(keymap-unset evil-insert-state-map "C-y" t)
(keymap-unset evil-insert-state-map "C-d" t)
(keymap-unset evil-insert-state-map "C-t" t)
(keymap-unset evil-insert-state-map "C-n" t)
(keymap-unset evil-insert-state-map "C-p" t)

(keymap-unset evil-motion-state-map "RET" t)
(keymap-unset evil-motion-state-map "SPC" t)

(keymap-set evil-motion-state-map "C-q" #'evil-record-macro)
(keymap-set evil-motion-state-map "q" #'quit-window)
(keymap-unset evil-normal-state-map "q" t)

(keymap-set evil-visual-state-map "m" #'evil-jump-item)
(keymap-set evil-operator-state-map "m" #'evil-jump-item)
(keymap-set evil-operator-state-map "o" #'evil-inner-symbol)
(keymap-set evil-operator-state-map "p" #'evil-inner-paragraph)

(keymap-set evil-motion-state-map "g f" 'find-file-at-point)
(keymap-set evil-motion-state-map "] f" 'find-file-at-point)
(keymap-set evil-motion-state-map "[ f" 'find-file-at-point)
(keymap-set evil-motion-state-map "g F" 'evil-find-file-at-point-with-line)
(keymap-set evil-motion-state-map "] F" 'evil-find-file-at-point-with-line)
(keymap-set evil-motion-state-map "[ F" 'evil-find-file-at-point-with-line)

(keymap-set evil-motion-state-map "g r" #'revert-buffer-quick)
(keymap-set evil-motion-state-map "g R" #'revert-buffer)

(set-keymap-parent evil-command-line-map minibuffer-local-map)

(defvar-keymap init-evil-override-mode-map)

(defun init-evil-keymap-set (state keymap &rest clauses)
  "Set evil key.
STATE KEYMAP CLAUSES see `evil-define-key*'."
  (declare (indent defun))
  (apply #'evil-define-key* state keymap
         (seq-map-indexed
          (lambda (v i)
            (if (cl-oddp i) v (kbd v)))
          clauses)))

(defun init-evil-minor-mode-keymap-set (state mode &rest clauses)
  "Set evil key in minor mode.
STATE MODE CLAUSES see `evil-define-minor-mode-key'."
  (declare (indent defun))
  (apply #'evil-define-minor-mode-key state mode
         (seq-map-indexed
          (lambda (v i)
            (if (cl-oddp i) v (kbd v)))
          clauses)))

(define-minor-mode init-evil-override-mode
  "Override leader prefix map."
  :group 'init-evil
  :global t
  :init-value t
  :keymap init-evil-override-mode-map)

;;;; extra

(require 'evil-surround)

(add-to-list 'evil-surround-pairs-alist '(?r . ("[" . "]")))
(add-to-list 'evil-surround-pairs-alist '(?a . ("<" . ">")))
(add-to-list 'evil-surround-pairs-alist '(?# . ("#{" . "}")))

(keymap-set evil-inner-text-objects-map "r" #'evil-inner-bracket)
(keymap-set evil-outer-text-objects-map "r" #'evil-a-bracket)
(keymap-set evil-inner-text-objects-map "a" #'evil-inner-angle)
(keymap-set evil-outer-text-objects-map "a" #'evil-an-angle)

(add-hook 'after-init-hook #'global-evil-surround-mode)

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

(keymap-set evil-normal-state-map "g c" #'init-evil-operator-comment)
(keymap-set evil-motion-state-map "g -" #'init-evil-operator-narrow)
(keymap-set evil-motion-state-map "g y" #'init-evil-operator-eval)

(evil-define-text-object init-evil-inner-line (count &optional _beg _end _type)
  (evil-range
   (save-excursion
     (goto-char (line-beginning-position))
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

(evil-define-text-object init-evil-text-object-entire (count &optional _beg _end _type)
  (evil-range (point-min) (point-max) 'line))

(keymap-set evil-inner-text-objects-map "l" #'init-evil-inner-line)
(keymap-set evil-outer-text-objects-map "l" #'init-evil-a-line)
(keymap-set evil-inner-text-objects-map "d" #'init-evil-inner-defun)
(keymap-set evil-outer-text-objects-map "d" #'init-evil-a-defun)
(keymap-set evil-inner-text-objects-map "h" #'init-evil-text-object-entire)
(keymap-set evil-outer-text-objects-map "h" #'init-evil-text-object-entire)

;;; files

(setq column-number-mode t)
(setq mode-line-percent-position '(6 "%q"))
(setq mode-line-position-line-format '(" %lL"))
(setq mode-line-position-column-format '(" %CC"))
(setq mode-line-position-column-line-format '(" %l:%C"))

(require 'files)

(setq version-control t)
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq delete-by-moving-to-trash t)

(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "save/" user-emacs-directory) t)))
(setq lock-file-name-transforms      `((".*" ,(expand-file-name "lock/" user-emacs-directory) t)))
(setq backup-directory-alist         `((".*" . ,(expand-file-name "backup/" user-emacs-directory))))

(keymap-set ctl-x-x-map "G" #'revert-buffer)

(keymap-set ctl-x-x-map "<left>" #'previous-buffer)
(keymap-set ctl-x-x-map "<right>" #'next-buffer)

(setq auto-save-visited-interval 0.5)

(defun init-auto-save-p ()
  "Predication of `auto-save-visited-mode'."
  (not (evil-insert-state-p)))

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

(require 'saveplace)

(add-hook 'after-init-hook #'save-place-mode)

(require 'so-long)

(add-hook 'after-init-hook #'global-so-long-mode)

;;;; vc

(require 'vc)
(require 'vc-git)

(setq vc-handled-backends '(Git))
(setq vc-display-status 'no-backend)
(setq vc-make-backup-files t)

(keymap-set ctl-x-x-map "v" #'vc-refresh-state)

(defvar init-git-program "git")
(defvar init-git-user-name "vhqr0")
(defvar init-git-user-email "zq_cmd@163.com")

(defun init-git-config-user ()
  "Init git repo."
  (interactive)
  (let ((directory default-directory)
        (buffer (get-buffer-create "*git-init*")))
    (save-window-excursion
      (with-current-buffer buffer
        (setq default-directory directory)
        (erase-buffer)
        (async-shell-command
         (format "%s config --local user.name %s && %s config --local user.email %s"
                 init-git-program init-git-user-name init-git-program init-git-user-email)
         (current-buffer))))))

;;;; project

(require 'project)

(setq project-mode-line t)
(setq project-switch-use-entire-map t)
(setq project-compilation-buffer-name-function #'project-prefixed-buffer-name)

(defvar-local init-find-test-file-name-function nil)

(defun init-project-find-test-file ()
  "Find test file or source file of current file in project."
  (interactive)
  (when init-find-test-file-name-function
    (when-let* ((default-directory (project-root (project-current))))
      (when-let* ((file-name (buffer-file-name)))
        (when-let* ((test-file-name (funcall init-find-test-file-name-function (file-relative-name file-name))))
          (find-file test-file-name))))))

(keymap-set project-prefix-map "t" #'init-project-find-test-file)

;;; window

(add-hook 'after-init-hook #'horizontal-scroll-bar-mode)

(define-minor-mode init-arrow-scroll-mode
  "Minor mode to use arrow keys to scroll around."
  :group 'init-window
  :global t
  :init-value t)

(init-evil-minor-mode-keymap-set 'motion 'init-arrow-scroll-mode
  "<left>" #'evil-scroll-left
  "<right>" #'evil-scroll-right
  "<up>" #'evil-scroll-up
  "<down>" #'evil-scroll-down)

(require 'windmove)

(windmove-default-keybindings)

(add-hook 'after-init-hook #'undelete-frame-mode)

(require 'tab-bar)

(setq tab-bar-position t)
(setq tab-bar-tab-hints t)
(setq tab-bar-select-tab-modifiers '(control meta))
(setq tab-bar-close-last-tab-choice 'delete-frame)

(add-to-list 'after-init-hook #'tab-bar-mode)
(add-to-list 'after-init-hook #'tab-bar-history-mode)

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

(require 'repeat)

(add-to-list 'after-init-hook #'repeat-mode)

(setq word-wrap-by-category t)

(setq-default indent-tabs-mode nil)

(defun init-indent-dwim ()
  "Do indent smartly."
  (interactive "*")
  (let ((bounds (init-region-or-buffer-bounds)))
    (indent-region (car bounds) (cdr bounds))))

(require 'elec-pair)

(add-to-list 'after-init-hook #'electric-pair-mode)

(require 'paren)

(setq show-paren-style 'expression)
(setq show-paren-context-when-offscreen 'child-frame)

(add-hook 'after-init-hook #'show-paren-mode)

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

(setq-default truncate-lines t)

(defun init-set-trailing-whitespace-display ()
  "Set local display of trailing whitespace."
  (setq-local show-trailing-whitespace t))

(add-hook 'text-mode-hook #'init-set-trailing-whitespace-display)
(add-hook 'prog-mode-hook #'init-set-trailing-whitespace-display)

(require 'display-line-numbers)

(defun init-toggle-line-numbers-relative ()
  "Toggle local display type of line numbers."
  (interactive)
  (setq-local display-line-numbers-type
              (if (eq display-line-numbers-type 'relative)
                  t
                'relative))
  (display-line-numbers-mode 1))

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(require 'hl-line)

(setq hl-line-sticky-flag t)

(defun init-narrow-to-block-placeholder ()
  "Placeholder to narrow to block command."
  (interactive)
  (user-error "No narrow to block command remap on placeholder"))

(defun init-narrow-to-subtree-placeholder ()
  "Placeholder to narrow to subtree command."
  (interactive)
  (user-error "No narrow to subtree command remap on placeholder"))

(keymap-set narrow-map "b" #'init-narrow-to-block-placeholder)
(keymap-set narrow-map "s" #'init-narrow-to-subtree-placeholder)

(defun init-jump-next-placeholder ()
  "Placeholder to jump next command."
  (interactive)
  (user-error "No jump next command remap on placeholder"))

(defun init-jump-previous-placeholder ()
  "Placeholder to jump previous command."
  (interactive)
  (user-error "No jump previous command remap on placeholder"))

(keymap-set evil-motion-state-map "C-j" #'init-jump-next-placeholder)
(keymap-set evil-motion-state-map "C-k" #'init-jump-previous-placeholder)

(require 'embark)

(keymap-global-set "M-o" #'embark-act)
(keymap-global-set "M-O" #'embark-act-all)

;;; isearch

(setq isearch-lazy-count t)
(setq isearch-allow-scroll t)
(setq isearch-allow-motion t)
(setq isearch-yank-on-move t)
(setq isearch-motion-changes-direction t)
;; (setq isearch-repeat-on-direction-change t)

(keymap-set evil-motion-state-map "/" #'isearch-forward-regexp)
(keymap-set evil-motion-state-map "?" #'isearch-backward-regexp)

(keymap-set embark-general-map "C-M-s" #'embark-isearch-forward)
(keymap-set embark-general-map "C-M-r" #'embark-isearch-backward)

(keymap-set embark-identifier-map "%" #'query-replace)

(defun init-evil-around-adjust-cursor-check-isearch (func &rest args)
  "Dont adjust cursor in isearch mode.
FUNC and ARGS see `evil-set-cursor'."
  (unless isearch-mode
    (apply func args)))

(advice-add #'evil-adjust-cursor :around #'init-evil-around-adjust-cursor-check-isearch)

(defvar-keymap init-evil-isearch-override-mode-map)

(define-minor-mode init-evil-isearch-override-mode
  "Override isearch commands map."
  :group 'init-evil
  :global t
  :init-value t
  :keymap init-evil-isearch-override-mode-map)

(defun init-isearch-filter (command)
  "Return COMMAND when isearch enabled."
  (when isearch-mode
    command))

(init-evil-keymap-set 'motion init-evil-isearch-override-mode-map
  "C-u" (init-filtered-command #'init-isearch-filter #'universal-argument)
  "C-f" (init-filtered-command #'init-isearch-filter #'forward-char)
  "C-b" (init-filtered-command #'init-isearch-filter #'backward-char)
  "C-a" (init-filtered-command #'init-isearch-filter #'move-beginning-of-line)
  "C-e" (init-filtered-command #'init-isearch-filter #'move-end-of-line))

(keymap-set occur-mode-map "<remap> <init-jump-next-placeholder>" #'next-error-no-select)
(keymap-set occur-mode-map "<remap> <init-jump-previous-placeholder>" #'previous-error-no-select)

(keymap-set occur-mode-map "C-c C-e" #'occur-edit-mode)

(evil-set-initial-state 'occur-edit-mode 'normal)

;;; input method

(keymap-global-set "C-SPC" #'toggle-input-method)
(keymap-global-set "C-@" #'toggle-input-method)
(keymap-set isearch-mode-map "C-SPC" #'isearch-toggle-input-method)
(keymap-set isearch-mode-map "C-@" #'isearch-toggle-input-method)

(defvar init-ignore-toggle-input-method nil)

(defun init-input-method-around-toggle-check-ignore (func &rest args)
  "Ignore toggle input method when `init-ignore-toggle-input-method' is set.
FUNC, ARGS see `f/activate-input-method' and `f/deactivate-input-method'."
  (unless init-ignore-toggle-input-method
    (apply func args)))

(advice-add #'toggle-input-method :around #'init-input-method-around-toggle-check-ignore)
(advice-add #'activate-input-method :around #'init-input-method-around-toggle-check-ignore)
(advice-add #'deactivate-input-method :around #'init-input-method-around-toggle-check-ignore)

(defun init-input-method-around-command-set-ignore (func &rest args)
  "Ignore toggle input method around command.
FUNC, ARGS see specified commands."
  (let ((init-ignore-toggle-input-method t))
    (apply func args)))

(dolist (func '(evil-local-mode
                evil-emacs-state
                evil-insert-state
                evil-replace-state
                evil-operator-state
                evil-motion-state
                evil-normal-state
                evil-visual-state))
  (advice-add func :around #'init-input-method-around-command-set-ignore))

(dolist (state '(operator motion normal visual))
  (setf (plist-get (cdr (assq state evil-state-properties)) :input-method) t))

(defun init-ignore-input-method-p ()
  "Predicate of input method."
  (and (not isearch-mode)
       evil-local-mode
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

(defun init-set-default-input-method ()
  "Set default input method function to `init-input-method'."
  (unless input-method-function
    (setq-local input-method-function #'init-input-method)))

(add-hook 'isearch-mode-hook #'init-set-default-input-method)
(advice-add #'isearch-toggle-input-method :after #'init-set-default-input-method)

;;; minibuffer

(setq enable-recursive-minibuffers t)
(setq read-extended-command-predicate #'command-completion-default-include-p)

(keymap-set minibuffer-local-map "<remap> <quit-window>" #'abort-recursive-edit)

(init-evil-keymap-set 'normal minibuffer-local-map
  "<escape>" #'abort-recursive-edit)

(require 'savehist)

(add-hook 'after-init-hook #'savehist-mode)

(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(require 'orderless)

(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides nil)

(require 'marginalia)

(add-hook 'after-init-hook #'marginalia-mode)

;;;; vertico

(defvar vertico-mode-map (make-sparse-keymap))

(require 'vertico)
(require 'vertico-multiform)
(require 'vertico-repeat)
(require 'vertico-suspend)
(require 'vertico-directory)

(setq vertico-resize nil)

(add-hook 'after-init-hook #'vertico-mode)
(add-hook 'after-init-hook #'vertico-multiform-mode)

(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

(keymap-set vertico-mode-map "C-c b" #'vertico-repeat)
(keymap-set vertico-mode-map "C-c z" #'vertico-suspend)

(defvar init-vertico-disable-commands '(kill-buffer))

(defun init-vertico-around-setup-filter-commands (func &rest args)
  "Disable vertico around `init-vertico-disable-commands'.
FUNC ARGS see `vertico--setup'."
  (unless (memq this-command init-vertico-disable-commands)
    (apply func args)))

(advice-add 'vertico--setup :around #'init-vertico-around-setup-filter-commands)

(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(keymap-set vertico-map "C-l" #'vertico-directory-up)

(keymap-set vertico-map "<remap> <evil-scroll-down>" #'vertico-scroll-up)
(keymap-set vertico-map "<remap> <evil-scroll-up>" #'vertico-scroll-down)
(keymap-set vertico-map "<remap> <evil-next-line>" #'vertico-next)
(keymap-set vertico-map "<remap> <evil-previous-line>" #'vertico-previous)
(keymap-set vertico-map "<remap> <evil-next-visual-line>" #'vertico-next)
(keymap-set vertico-map "<remap> <evil-previous-visual-line>" #'vertico-previous)
(keymap-set vertico-map "<remap> <evil-goto-first-line>" #'vertico-first)
(keymap-set vertico-map "<remap> <evil-goto-line>" #'vertico-last)
(keymap-set vertico-map "<remap> <init-jump-next-placeholder>" #'vertico-next-group)
(keymap-set vertico-map "<remap> <init-jump-previous-placeholder>" #'vertico-previous-group)

(keymap-set vertico-map "C-x C-s" #'embark-export)

;;; search

(require 'consult)
(require 'embark-consult)

(setq consult-preview-key '(:debounce 0.3 any))

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

;;;; line

(consult-customize consult-goto-line :preview-key 'any)

(keymap-set init-consult-override-mode-map "<remap> <goto-line>" #'consult-goto-line)

(setq consult-line-start-from-top t)

(consult-customize consult-line :preview-key 'any)

(defun init-consult-line-after-search-set-history (&rest _args)
  "Set search history after `consult-line'."
  (let ((search (car consult--line-history)))
    (add-to-history 'regexp-search-ring search regexp-search-ring-max)
    (setq isearch-string search)
    (setq isearch-regexp t)
    (setq isearch-forward t)))

(advice-add #'consult-line :after #'init-consult-line-after-search-set-history)

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
  (let ((thing (init-thing-at-point)))
    (deactivate-mark)
    (init-consult-search arg thing)))

(keymap-set search-map "s" #'init-consult-search)
(keymap-global-set "C-s" #'init-consult-search-dwim)

(keymap-set embark-general-map "C-s" #'consult-line)
(keymap-set embark-general-map "C-r" #'consult-line)

;;;; imenu

(require 'consult-imenu)

(consult-customize consult-imenu :preview-key 'any)

(keymap-set init-consult-override-mode-map "<remap> <imenu>" #'consult-imenu)

;;;; outline

(defvar init-consult-outline-history nil)

(defun init-consult-outline-candidates ()
  "Collect outline headings."
  (consult--forbid-minibuffer)
  (let ((bol-regex (concat "^\\(?:" outline-regexp "\\)"))
        (stack-level 0)
        stack cands line name level marker)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward bol-regex nil t)
        (save-excursion
          (setq line (line-number-at-pos))
          (setq name (buffer-substring-no-properties (point) (line-end-position)))
          (goto-char (match-beginning 0))
          (setq marker (point-marker))
          (setq level (funcall outline-level))
          (while (<= level stack-level)
            (pop stack)
            (setq stack-level (1- stack-level)))
          (while (> level stack-level)
            (push "" stack)
            (setq stack-level (1+ stack-level)))
          (setq stack (cons name (cdr stack)))
          (let* ((sep (propertize " / " 'face 'consult-line-number))
                 (name (concat
                        (propertize (format "%5d " line) 'face 'consult-line-number)
                        (mapconcat #'identity (reverse stack) sep))))
            (push (cons name marker) cands)))))
    (nreverse cands)))

(defun init-consult-outline-state ()
  "Construct state for `init-consult-outline'."
  (let ((jump (consult--jump-state)))
    (lambda (action cand)
      (funcall jump action (cdr cand)))))

(defun init-consult-outline ()
  "Enhanced version of `consult-outline' using counsel functionality."
  (interactive)
  (let ((candidate (consult--read
                    (consult--slow-operation
                        "Collecting headings..."
                      (init-consult-outline-candidates))
                    :prompt "Goto outline heading: "
                    :state (init-consult-outline-state)
                    :lookup #'consult--lookup-cons
                    :sort nil
                    :require-match t
                    :history 'init-consult-outline-history
                    :add-history (thing-at-point 'symbol))))
    (goto-char (cdr candidate))))

(consult-customize init-consult-outline :preview-key 'any)

;;; goggles

(defvar-local init-goggles-changes nil)

(defun init-goggles-pre-command ()
  "Reset change."
  (setq init-goggles-changes nil))

(defun init-goggles-post-command ()
  "Highlight change post command."
  (when init-goggles-changes
    (let ((start most-positive-fixnum)
          (end 0))
      (dolist (change init-goggles-changes)
        (setq start (min start (car change)))
        (setq end (max end (cdr change)))
        (set-marker (car change) nil)
        (set-marker (cdr change) nil))
      (pulse-momentary-highlight-region start end))
    (setq init-goggles-changes nil)))

(defun init-goggles-after-change (start end len)
  "Push change to `init-goggles-changes'.
START END LEN see `after-change-functions'."
  (when (and (/= len 0) (= start end))
    (when (> start (buffer-size))
      (setq start (- start 1)))
    (setq end (1+ start)))
  (let ((change (cons (copy-marker start) (copy-marker end))))
    (push change init-goggles-changes)))

(define-minor-mode init-goggles-mode
  "Init goggles mode."
  :group 'init-goggles
  (if init-goggles-mode
      (progn
        (add-hook 'pre-command-hook #'init-goggles-pre-command nil t)
        (add-hook 'post-command-hook #'init-goggles-post-command nil t)
        (add-hook 'after-change-functions #'init-goggles-after-change nil t))
    (remove-hook 'pre-command-hook #'init-goggles-pre-command t)
    (remove-hook 'post-command-hook #'init-goggles-post-command t)
    (remove-hook 'after-change-functions #'init-goggles-after-change t)))

(add-hook 'prog-mode-hook #'init-goggles-mode)
(add-hook 'text-mode-hook #'init-goggles-mode)
(add-hook 'minibuffer-mode-hook #'init-goggles-mode)

(defun init-evil-around-operator-goggles (func beg end &rest args)
  "Around evil operator do goggles.
FUNC BEG END ARGS see `evil-yank', `evil-delete', etc."
  (when (and init-goggles-mode (called-interactively-p 'interactive))
    (pulse-momentary-highlight-region beg end)
    (sit-for 0.05))
  (apply func beg end args))

(defvar init-evil-goggles-commands
  '(evil-yank
    evil-delete
    evil-change
    evil-indent
    evil-shift-right
    evil-shift-left
    evil-fill-and-move
    evil-surround-region
    evil-Surround-region
    init-evil-operator-comment
    init-evil-operator-narrow
    init-evil-operator-eval))

(dolist (command init-evil-goggles-commands)
  (advice-add command :around #'init-evil-around-operator-goggles))

;;; spell

(require 'ispell)

(setq ispell-dictionary "american")

;;; ibuffer

(require 'ibuffer)
(require 'ibuf-ext)

(setq ibuffer-formats
      '((mark modified read-only locked
              " " (name 40 40 :left :elide)
	      " " (size 9 -1 :right)
	      " " (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " " (name 40 -1) " " filename)))

;;; dired

(require 'dired)
(require 'dired-x)
(require 'wdired)

(setq dired-dwim-target t)
(setq dired-auto-revert-buffer t)
(setq dired-listing-switches "-lha")

(put 'dired-jump 'repeat-map nil)

(keymap-set ctl-x-4-map "j" #'dired-jump-other-window)
(keymap-set project-prefix-map "j" #'project-dired)

(keymap-set dired-mode-map "<remap> <evil-next-line>" #'dired-next-line)
(keymap-set dired-mode-map "<remap> <evil-previous-line>" #'dired-previous-line)
(keymap-set dired-mode-map "<remap> <evil-next-visual-line>" #'dired-next-line)
(keymap-set dired-mode-map "<remap> <evil-previous-visual-line>" #'dired-previous-line)

(keymap-set dired-mode-map "C-c C-e" #'wdired-change-to-wdired-mode)

(init-evil-keymap-set 'normal dired-mode-map
  "m" #'dired-mark
  "u" #'dired-unmark
  "U" #'dired-unmark-all-marks
  "d" #'dired-flag-file-deletion
  "x" #'dired-do-flagged-delete
  "s" #'dired-sort-toggle-or-edit
  "D" #'dired-do-delete
  "C" #'dired-do-copy
  "R" #'dired-do-rename
  "+" #'dired-create-directory
  "=" #'dired-diff
  "!" #'dired-do-shell-command
  "&" #'dired-do-async-shell-command)

;;; archive

(require 'arc-mode)

(keymap-set archive-mode-map "<remap> <evil-next-line>" #'archive-next-line)
(keymap-set archive-mode-map "<remap> <evil-previous-line>" #'archive-previous-line)
(keymap-set archive-mode-map "<remap> <evil-next-visual-line>" #'archive-next-line)
(keymap-set archive-mode-map "<remap> <evil-previous-visual-line>" #'archive-previous-line)

;;; process

;;;; compile

(require 'compile)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(keymap-set project-prefix-map "C" #'project-recompile)

(keymap-set compilation-mode-map "<remap> <init-jump-next-placeholder>" #'next-error-no-select)
(keymap-set compilation-mode-map "<remap> <init-jump-previous-placeholder>" #'previous-error-no-select)
(keymap-set compilation-minor-mode-map "<remap> <init-jump-next-placeholder>" #'next-error-no-select)
(keymap-set compilation-minor-mode-map "<remap> <init-jump-previous-placeholder>" #'previous-error-no-select)

;;;; grep

(require 'grep)

(keymap-set grep-mode-map "<remap> <init-jump-next-placeholder>" #'next-error-no-select)
(keymap-set grep-mode-map "<remap> <init-jump-previous-placeholder>" #'previous-error-no-select)

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
  (let* ((default-directory (init-directory-interactive arg "Search directory: "))
         (pattern-default (init-thing-at-point))
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

(keymap-set comint-mode-map "<remap> <init-jump-next-placeholder>" #'comint-next-prompt)
(keymap-set comint-mode-map "<remap> <init-jump-previous-placeholder>" #'comint-previous-prompt)

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

(keymap-set eshell-mode-map "<remap> <init-jump-next-placeholder>" #'eshell-next-prompt)
(keymap-set eshell-mode-map "<remap> <init-jump-previous-placeholder>" #'eshell-previous-prompt)

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
ARG see `init-switch-to-buffer-split-window-interactive'."
  (interactive "P")
  (init-switch-to-buffer-split-window-interactive arg (init-eshell-dwim-get-buffer-create)))

;;; vc

;;;; vc modes

(require 'vc-dir)

(keymap-set vc-dir-mode-map "<remap> <evil-next-line>" #'vc-dir-next-line)
(keymap-set vc-dir-mode-map "<remap> <evil-previous-line>" #'vc-dir-previous-line)
(keymap-set vc-dir-mode-map "<remap> <evil-next-visual-line>" #'vc-dir-next-line)
(keymap-set vc-dir-mode-map "<remap> <evil-previous-visual-line>" #'vc-dir-previous-line)

(keymap-set vc-prefix-map "p" #'vc-push)

(init-evil-keymap-set 'motion vc-dir-mode-map
  "p" #'vc-push)

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

(defun init-ediff-setup-keymap-extra ()
  "Setup extra ediff bindings."
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

(advice-add #'ediff-setup-keymap :after #'init-ediff-setup-keymap-extra)

;;; image

(require 'image)

(keymap-set image-slice-map "i =" #'image-increase-size)
(keymap-set image-slice-map "=" #'image-increase-size)
(keymap-set image-slice-map "+" #'image-increase-size)
(keymap-set image-slice-map "-" #'image-decrease-size)
(keymap-set image-slice-map "r" #'image-rotate)

(keymap-set image--repeat-map "=" #'image-increase-size)

(init-evil-keymap-set '(motion normal) image-slice-map
  "=" #'image-increase-size
  "+" #'image-increase-size
  "-" #'image-decrease-size
  "r" #'image-rotate)

(require 'image-mode)

(keymap-set image-mode-map "a =" #'image-increase-speed)

(keymap-set image-mode-map "C-=" #'image-increase-size)
(keymap-set image-mode-map "C-+" #'image-increase-size)
(keymap-set image-mode-map "C--" #'image-decrease-size)

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
(keymap-set image-mode-map "<remap> <init-jump-next-placeholder>" #'image-next-file)
(keymap-set image-mode-map "<remap> <init-jump-previous-placeholder>" #'image-previous-file)

;;; prog

;;;; flymake

(require 'flymake)
(require 'flymake-proc)

(remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)

(setq flymake-no-changes-timeout 1)
(setq flymake-show-diagnostics-at-end-of-line 'short)

(keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
(keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error)

;;;; eldoc

(require 'eldoc)

(setq eldoc-minor-mode-string nil)

(keymap-set prog-mode-map "<remap> <display-local-help>" #'eldoc-doc-buffer)

;;;; xref

(require 'xref)

(setq xref-search-program 'ripgrep)

(keymap-set xref--xref-buffer-mode-map "<remap> <init-jump-next-placeholder>" #'xref-next-line)
(keymap-set xref--xref-buffer-mode-map "<remap> <init-jump-previous-placeholder>" #'xref-prev-line)

;;;; abbrev

(require 'abbrev)

(setq-default abbrev-mode t)

(init-diminish-minor-mode 'abbrev-mode)

(defvar yas-alias-to-yas/prefix-p)
(setq yas-alias-to-yas/prefix-p nil)

(require 'yasnippet)

(init-diminish-minor-mode 'yas-minor-mode)

(add-hook 'after-init-hook #'yas-global-mode)

(defun init-define-yas-abbrev (table abbrev snippet &optional env)
  "Define an ABBREV in TABLE, to expand a yas SNIPPET with ENV."
  (let ((length (length abbrev))
        (hook (make-symbol abbrev)))
    (put hook 'no-self-insert t)
    (fset hook (lambda ()
                 (let ((point (point)))
                   (yas-expand-snippet snippet (- point length) point env))))
    (define-abbrev table abbrev 'yas hook :system t)))

(defun init-define-abbrev (table abbrev expansion)
  "Define an ABBREV in TABLE, to expand as EXPANSION.
EXPANSION may be:
- text: (text \"expansion\")
- yas: (yas \"expansion\" ENVSYM ENVVAL...)"
  (let ((expansion-type (car expansion))
        (expansion (cdr expansion)))
    (cond ((eq expansion-type 'text)
           (define-abbrev table abbrev (car expansion) nil :system t))
          ((eq expansion-type 'yas)
           (init-define-yas-abbrev table abbrev (car expansion) (cdr expansion)))
          (t
           (user-error "Invalid abbrev expansion type")))))

(defun init-define-abbrev-table (tablename defs)
  "Define abbrev table with TABLENAME and abbrevs DEFS."
  (let ((table (if (boundp tablename) (symbol-value tablename))))
    (unless table
      (setq table (make-abbrev-table))
      (set tablename table))
    (unless (memq tablename abbrev-table-name-list)
      (push tablename abbrev-table-name-list))
    (dolist (def defs)
      (init-define-abbrev table (car def) (cdr def)))))

(defvar init-abbrevs-file
  (expand-file-name "abbrevs.eld" priv-directory))

(defun init-load-abbrevs (&optional file)
  "Load abbrevs FILE."
  (interactive)
  (let ((file (or file init-abbrevs-file)))
    (when (file-exists-p file)
      (let ((defs (read (init-read-file-contents file))))
        (dolist (def defs)
          (init-define-abbrev-table (car def) (cdr def)))))))

(add-hook 'after-init-hook #'init-load-abbrevs)

;;;; company

(require 'company)
(require 'company-files)
(require 'company-capf)
(require 'company-keywords)
(require 'company-dabbrev)
(require 'company-dabbrev-code)

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

(add-hook 'after-init-hook #'global-company-mode)

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

(defun init-company-around-capf-set-styles (func &rest args)
  "Set completion styles for `company-capf'.
FUNC ARGS see `company-capf'."
  (let ((completion-styles '(basic partial-completion)))
    (apply func args)))

(advice-add 'company-capf :around #'init-company-around-capf-set-styles)

(defun init-company-around-call-backend-check-evil (func command &rest args)
  "Check evil state before call company.
FUNC COMMAND ARGS see `company-call-backend'."
  (unless (and evil-mode (eq evil-state 'normal) (eq command 'prefix))
    (apply func command args)))

(advice-add #'company-call-backend :around #'init-company-around-call-backend-check-evil)

;;;; eglot

(require 'eglot)

(setq eglot-extend-to-xref t)

(keymap-set eglot-mode-map "<remap> <evil-lookup>" #'eldoc-doc-buffer)

;;; elisp

;;;; lisp

(defun init-lisp-outline-level ()
  "Return level of current outline heading."
  (if (looking-at ";;\\([;*]+\\)")
      (- (match-end 1) (match-beginning 1))
    (funcall outline-level)))

(defun init-lisp-set-outline ()
  "Set outline vars."
  (setq-local outline-regexp ";;[;*]+[\s\t]+")
  (setq-local outline-level #'init-lisp-outline-level)
  (outline-minor-mode 1))

(defun init-lisp-around-last-sexp-maybe-forward (func &rest args)
  "Around *-last-sexp command.
Save point and forward sexp before command if looking at an open paren.
FUNC and ARGS see specific command."
  (save-excursion
    (when (looking-at-p "(\\|\\[\\|{")
      (forward-sexp))
    (apply func args)))

;;;; elisp

(defvar init-elisp-last-sexp-commands
  '(eval-last-sexp eval-print-last-sexp pp-eval-last-sexp pp-macroexpand-last-sexp))

(dolist (command init-elisp-last-sexp-commands)
  (advice-add command :around #'init-lisp-around-last-sexp-maybe-forward))

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (keymap-set map "C-c C-k" #'eval-buffer)
  (keymap-set map "C-C C-l" #'load-file)
  (keymap-set map "C-c C-m" #'pp-macroexpand-last-sexp))

(add-hook 'emacs-lisp-mode-hook #'init-lisp-set-outline)

(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (add-to-list 'init-evil-eval-function-alist `(,mode . eval-region)))

;;;; flymake

(setq trusted-content (list (file-name-as-directory (abbreviate-file-name init-lisp-directory))))

(setq elisp-flymake-byte-compile-load-path load-path)

(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

;;;; ielm

(require 'ielm)

(defun init-ielm-other-window ()
  "Switch to elisp repl other window."
  (interactive)
  (pop-to-buffer (get-buffer-create "*ielm*"))
  (ielm))

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (keymap-set map "C-c C-z" #'init-ielm-other-window))

;;;; lookup

(defun init-describe-symbol-dwim ()
  "Describe symbol at point."
  (interactive)
  (describe-symbol (intern (init-thing-at-point-or-throw))))

(setq evil-lookup-func #'init-describe-symbol-dwim)

;;;; load

(defvar-keymap init-load-map
  "f" #'load-file
  "l" #'load-library
  "t" #'load-theme)

(keymap-set help-map "t" init-load-map)

(consult-customize consult-theme :preview-key '(:debounce 0.5 any))
(keymap-set init-consult-override-mode-map "<remap> <load-theme>" #'consult-theme)

;;;; find

(require 'find-func)

;; TODO remove special handle after 31.1
(when (fboundp 'find-function-mode)
  (add-hook 'after-init-hook 'find-function-mode))

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

;;;; help

(keymap-set help-map "B" #'describe-keymap)
(keymap-set help-map "p" #'describe-package)
(keymap-set help-map "P" #'finder-by-keyword)

;;; leaders

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
  "v" #'visual-line-mode
  "w" #'whitespace-mode
  "W" #'whitespace-newline-mode
  "h" #'hl-line-mode
  "n" #'display-line-numbers-mode
  "N" #'init-toggle-line-numbers-relative
  "f" #'follow-mode
  "d" #'eldoc-doc-buffer
  "e" #'flymake-show-buffer-diagnostics
  "E" #'flymake-show-project-diagnostics)

(init-leader-set
 "SPC" #'consult-buffer
 "\\" #'init-magic-shift
 "TAB" #'init-magic-shift
 "<tab>" #'init-magic-shift
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
 "k" #'kill-buffer
 "f" #'find-file
 "d" #'dired
 "j" #'dired-jump
 "B" #'ibuffer
 "S" #'init-rg-dwim
 "e" #'init-eshell-dwim
 "w" evil-window-map
 "4" ctl-x-4-map
 "5" ctl-x-5-map
 "t" tab-prefix-map
 "p" project-prefix-map
 "v" vc-prefix-map
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
 "i" #'imenu
 "l" #'init-consult-outline
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
