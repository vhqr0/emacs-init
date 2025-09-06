;;; init-emacs.el --- Init Emacs -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Emacs itself.

(require 'dash)
(require 'init-core)

;;; Code:

;;; essentials

(prefer-coding-system 'utf-8)

(setq system-time-locale "C")

(setq read-process-output-max (* 1024 1024))

;;; utils

(defun init-diminish-minor-mode (mode)
  "Diminish MODE lighter."
  (setq minor-mode-alist
        (->> minor-mode-alist
             (--remove (eq (car it) mode)))))

(defun init-region-content ()
  "Get region content or nil."
  (when (region-active-p)
    (buffer-substring (region-beginning) (region-end))))

(defun init-thing-at-point ()
  "Get thing at point dwim."
  (or (init-region-content) (thing-at-point 'symbol)))

(defun init-thing-at-point-or-throw ()
  "Get thing at point dwim, or throw user error."
  (or (init-thing-at-point) (user-error "No thing at point")))

(defun init-project-directory ()
  "Get current project directory."
  (-when-let (project (project-current))
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

;;; files

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

;;;; autosave

(setq auto-save-visited-interval 0.5)

(add-to-list 'minor-mode-alist '(auto-save-visited-mode " ASave"))

(defvar init-disable-autosave-preds nil)

(defun init-autosave-p ()
  "Predication of `auto-save-visited-mode'."
  (not (run-hook-with-args-until-success 'init-disable-autosave-preds)))

(setq auto-save-visited-predicate #'init-autosave-p)

(add-hook 'after-init-hook #'auto-save-visited-mode)

;;;; autorevert

(require 'autorevert)

(setq auto-revert-check-vc-info t)

(add-hook 'after-init-hook #'global-auto-revert-mode)

;;;; recentf

(require 'recentf)

(setq recentf-max-saved-items 500)

(add-hook 'after-init-hook #'recentf-mode)

(keymap-set ctl-x-r-map "e" #'recentf-open)

;;;; saveplace

(require 'saveplace)

(add-hook 'after-init-hook #'save-place-mode)

;;;; solong

(require 'so-long)

(add-hook 'after-init-hook #'global-so-long-mode)

;;;; vc

(require 'vc)
(require 'vc-git)

(setq vc-handled-backends '(Git))
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

(keymap-set project-prefix-map "j" #'project-dired)
(keymap-set project-prefix-map "C" #'project-recompile)

(defvar-local init-find-test-file-name-function nil)

(defun init-project-find-test-file ()
  "Find test file or source file of current file in project."
  (interactive)
  (when init-find-test-file-name-function
    (-when-let (default-directory (project-root (project-current)))
      (-when-let (file-name (buffer-file-name))
        (-when-let (test-file-name (funcall init-find-test-file-name-function (file-relative-name file-name)))
          (find-file test-file-name))))))

(keymap-set project-prefix-map "t" #'init-project-find-test-file)

;;; ui

;;;; graphic

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

(setq ring-bell-function #'ignore)

(defvar init-disable-ui-modes
  '(blink-cursor-mode tooltip-mode tool-bar-mode menu-bar-mode scroll-bar-mode))

(defun init-disable-ui ()
  "Disable various ui modes."
  (interactive)
  (dolist (mode init-disable-ui-modes)
    (when (fboundp mode)
      (funcall mode -1))))

(add-hook 'after-init-hook #'init-disable-ui)

;;;; windows

(require 'windmove)

(windmove-default-keybindings)

(require 'tab-bar)

(setq tab-bar-tab-hints t)
(setq tab-bar-select-tab-modifiers '(control meta))
(setq tab-bar-close-last-tab-choice 'delete-frame)

(add-to-list 'after-init-hook #'tab-bar-mode)
(add-to-list 'after-init-hook #'tab-bar-history-mode)

(defvar-keymap init-tab-bar-history-repeat-map
  :repeat t
  "<left>" #'tab-bar-history-back
  "<right>" #'tab-bar-history-forward)

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

;;;; commands

(setq disabled-command-function nil)
(setq suggest-key-bindings nil)

(keymap-global-set "C-SPC" #'toggle-input-method)

(require 'repeat)

(add-to-list 'after-init-hook #'repeat-mode)

(require 'embark)

(keymap-global-set "M-o" #'embark-act)
(keymap-global-set "M-O" #'embark-act-all)

;;;; indent

(setq-default indent-tabs-mode nil)

(setq-default truncate-lines t)

(setq word-wrap-by-category t)

(defun init-indent-dwim ()
  "Do indent smartly."
  (interactive "*")
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (indent-region (point-min) (point-max))))

;;;; parens

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

(defun init-wrap-pair (&optional arg)
  "Insert pair, ARG see `insert-pair'."
  (interactive "*P")
  (insert-pair (or arg 1))
  (indent-sexp))

;;;; visual

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

;;;; goggles

(defvar init-goggles-buffer nil)
(defvar init-goggles-changes nil)

(defun init-goggles-pre-command ()
  "Save current buffer."
  (setq init-goggles-buffer (current-buffer))
  (setq init-goggles-changes nil))

(defun init-goggles-post-command ()
  "Highlight change post command."
  (when (and init-goggles-changes
             (eq (current-buffer) init-goggles-buffer))
    (let ((start most-positive-fixnum)
          (end 0))
      (dolist (change init-goggles-changes)
        (when (eq init-goggles-buffer (marker-buffer (car change)))
          (setq start (min start (car change)))
          (setq end (max end (cdr change)))
          (set-marker (car change) nil)
          (set-marker (cdr change) nil)))
      (pulse-momentary-highlight-region start end)))
  (setq init-goggles-buffer nil)
  (setq init-goggles-changes nil))

(defun init-goggles-after-change (start end len)
  "Push change to history.
START END LEN see `after-change-functions'."
  (when (eq (current-buffer) init-goggles-buffer)
    (when (and (/= len 0) (= start end))
      (when (> start (buffer-size))
        (setq start (- start 1)))
      (setq end (1+ start)))
    (let ((change (cons (copy-marker start) (copy-marker end))))
      (push change init-goggles-changes))))

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

;;; evil

;; keybinding related options, must be set before evil loaded.

(defvar evil-want-keybinding)
(setq evil-want-keybinding nil)

(defvar evil-want-minibuffer)
(setq evil-want-minibuffer t)

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

(setq evil-want-fine-undo t)
(setq evil-search-wrap nil)
(setq evil-symbol-word-search t)

(setq evil-goto-definition-functions
      '(evil-goto-definition-imenu evil-goto-definition-xref))

(setq evil-emacs-state-modes nil)
(setq evil-insert-state-modes nil)
(setq evil-motion-state-modes nil)

(add-hook 'after-init-hook #'evil-mode)

(defvar init-evil-disable-adjust-cursor-commands
  '(forward-sexp forward-list))

(defun init-around-evil-adjust-cursor-do-filter (func &rest args)
  "Dont adjust cursor after certain commands.
FUNC and ARGS see `evil-set-cursor'."
  (unless (memq this-command init-evil-disable-adjust-cursor-commands)
    (apply func args)))

(advice-add #'evil-adjust-cursor :around #'init-around-evil-adjust-cursor-do-filter)

(defun init-around-evil-adjust-cursor-do-disable-isearch (func &rest args)
  "Dont adjust cursor in isearch mode.
FUNC and ARGS see `evil-set-cursor'."
  (unless isearch-mode
    (apply func args)))

(advice-add #'evil-adjust-cursor :around #'init-around-evil-adjust-cursor-do-disable-isearch)

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

(keymap-set evil-motion-state-map "-" #'negative-argument)
(keymap-set evil-motion-state-map "C-q" #'evil-record-macro)
(keymap-set evil-motion-state-map "q" #'quit-window)
(keymap-unset evil-normal-state-map "q" t)

(keymap-set evil-visual-state-map "m" #'evil-jump-item)
(keymap-set evil-operator-state-map "m" #'evil-jump-item)
(keymap-set evil-operator-state-map "o" #'evil-inner-symbol)
(keymap-set evil-operator-state-map "p" #'evil-inner-paragraph)

(keymap-set evil-normal-state-map "M-r" #'raise-sexp)
(keymap-set evil-normal-state-map "M-s" #'paredit-splice-sexp)

(keymap-set evil-motion-state-map "g r" #'revert-buffer-quick)
(keymap-set evil-motion-state-map "g R" #'revert-buffer)

(keymap-set evil-window-map "<left>" #'tab-bar-history-back)
(keymap-set evil-window-map "<right>" #'tab-bar-history-forward)

(set-keymap-parent evil-command-line-map minibuffer-local-map)

(add-to-list 'init-disable-autosave-preds #'evil-insert-state-p)

;;;; surround

(require 'evil-surround)

(add-to-list 'evil-surround-pairs-alist '(?r . ("[" . "]")))
(add-to-list 'evil-surround-pairs-alist '(?a . ("<" . ">")))
(add-to-list 'evil-surround-pairs-alist '(?# . ("#{" . "}")))

(keymap-set evil-inner-text-objects-map "r" #'evil-inner-bracket)
(keymap-set evil-outer-text-objects-map "r" #'evil-a-bracket)
(keymap-set evil-inner-text-objects-map "a" #'evil-inner-angle)
(keymap-set evil-outer-text-objects-map "a" #'evil-an-angle)

(add-hook 'after-init-hook #'global-evil-surround-mode)

;;;; extra

;;;;; quick

(defun init-quick-event (first-char second-char quick-event)
  "Quick trigger QUICK-EVENT by repeat click two keys: FIRST-CHAR SECOND-CHAR."
  (if (or executing-kbd-macro
          defining-kbd-macro
          (sit-for 0.15 t))
      (insert first-char)
    (let ((event (read-event)))
      (if (= event second-char)
          (progn
            (setq this-command #'ignore)
            (setq real-this-command #'ignore)
            (push quick-event unread-command-events))
        (insert first-char)
        (push event unread-command-events)))))

(defun init-evil-escape ()
  ":imap jk <esc>."
  (interactive)
  (init-quick-event ?j ?k 'escape))

(keymap-set evil-insert-state-map  "j" #'init-evil-escape)
(keymap-set evil-replace-state-map "j" #'init-evil-escape)

;;;;; operators

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
  (-when-let (eval-function (cdr (assq major-mode init-evil-eval-function-alist)))
    (funcall eval-function beg end)))

(keymap-set evil-normal-state-map "g c" #'init-evil-operator-comment)
(keymap-set evil-motion-state-map "g -" #'init-evil-operator-narrow)
(keymap-set evil-motion-state-map "g y" #'init-evil-operator-eval)

;;;;; tobjs

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

;;;;; override

(defvar-keymap init-evil-override-mode-map)

(define-minor-mode init-evil-override-mode
  "Override leader prefix map."
  :group 'init-evil
  :global t
  :keymap init-evil-override-mode-map)

(add-hook 'after-init-hook #'init-evil-override-mode)

;;; completion

;;;; minibuffer

(setq enable-recursive-minibuffers t)

(keymap-set minibuffer-local-map "<remap> <quit-window>" #'abort-recursive-edit)

(evil-define-key 'insert minibuffer-local-map
  (kbd "M-r") #'previous-matching-history-element)

(evil-define-key 'normal minibuffer-local-map
  (kbd "<escape>") #'abort-recursive-edit
  (kbd "RET") #'exit-minibuffer
  (kbd "<return>") #'exit-minibuffer)

;;;; savehist

(require 'savehist)

(add-hook 'after-init-hook #'savehist-mode)

;;;; isearch

(setq isearch-lazy-count t)
(setq isearch-allow-scroll t)
(setq isearch-allow-motion t)
(setq isearch-yank-on-move t)
(setq isearch-motion-changes-direction t)
(setq isearch-repeat-on-direction-change t)

(defun init-isearch-menu-item-filter (command)
  "Return COMMAND when isearch enabled."
  (when isearch-mode
    command))

(defun init-isearch-menu-item-filter-wrap (command)
  "Wrap COMMAND with `init-isearch-menu-item-filter'."
  `(menu-item "" ,command :filter init-isearch-menu-item-filter))

(evil-define-key 'motion init-evil-override-mode-map
  (kbd "C-f") (init-isearch-menu-item-filter-wrap #'forward-char)
  (kbd "C-b") (init-isearch-menu-item-filter-wrap #'backward-char)
  (kbd "C-a") (init-isearch-menu-item-filter-wrap #'move-beginning-of-line)
  (kbd "C-e") (init-isearch-menu-item-filter-wrap #'move-end-of-line))

(keymap-set embark-identifier-map "%" #'query-replace)
(keymap-set embark-general-map "C-M-s" #'embark-isearch-forward)
(keymap-set embark-general-map "C-M-r" #'embark-isearch-backward)

;;;; styles

(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-extended-command-predicate #'command-completion-default-include-p)

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

(keymap-set vertico-map "C-l" #'vertico-directory-up)

(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(defvar init-vertico-disable-commands '(kill-buffer))

(defun init-around-vertico-setup-do-filter (func &rest args)
  "Disable vertico around `init-vertico-disable-commands'.
FUNC ARGS see `vertico--setup'."
  (unless (memq this-command init-vertico-disable-commands)
    (apply func args)))

(advice-add 'vertico--setup :around #'init-around-vertico-setup-do-filter)

(keymap-set vertico-map "C-x C-s" #'embark-export)

(evil-define-key 'normal vertico-map
  "j" #'vertico-next
  "k" #'vertico-previous
  "gj" #'vertico-next-group
  "gk" #'vertico-previous-group
  (kbd "C-j") #'vertico-next-group
  (kbd "C-k") #'vertico-previous-group
  "gg" #'vertico-first
  "G" #'vertico-last
  (kbd "C-u") #'vertico-scroll-down
  (kbd "C-d") #'vertico-scroll-up)

;;;; consult

(require 'consult)
(require 'embark-consult)

(setq consult-preview-key '(:debounce 0.3 any))

(setq completion-in-region-function #'consult-completion-in-region)

(defvar-keymap init-consult-override-mode-map)

(define-minor-mode init-consult-override-mode
  "Override consult commands."
  :group 'init-consult
  :global t
  :keymap init-consult-override-mode-map)

(add-hook 'after-init-hook #'init-consult-override-mode)

;;;;; history

(keymap-set init-consult-override-mode-map "<remap> <yank>" #'consult-yank-from-kill-ring)
(keymap-set init-consult-override-mode-map "<remap> <yank-pop>" #'consult-yank-pop)
(keymap-set init-consult-override-mode-map "<remap> <previous-matching-history-element>" #'consult-history)
(keymap-set init-consult-override-mode-map "<remap> <eshell-previous-matching-input>" #'consult-history)
(keymap-set init-consult-override-mode-map "<remap> <comint-history-isearch-backward-regexp>" #'consult-history)

;;;;; search

(require 'consult-imenu)

(setq consult-line-start-from-top t)

(consult-customize
 consult-goto-line
 consult-imenu
 consult-line
 :preview-key 'any)

(keymap-set init-consult-override-mode-map "<remap> <goto-line>" #'consult-goto-line)
(keymap-set init-consult-override-mode-map "<remap> <imenu>" #'consult-imenu)

(defun init-after-consult-line-set-search (&rest _args)
  "Set search history after `consult-line'."
  (let ((search (car consult--line-history)))
    (add-to-history 'regexp-search-ring search regexp-search-ring-max)
    (setq isearch-string search)
    (setq isearch-regexp t)
    (setq isearch-forward t)))

(advice-add #'consult-line :after #'init-after-consult-line-set-search)

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

;;;;; outline

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

;;; help

(evil-set-initial-state 'help-mode 'motion)

(evil-define-key 'motion help-mode-map
  (kbd "TAB") #'forward-button
  (kbd "S-TAB") #'backward-button
  (kbd "<tab>") #'forward-button
  (kbd "<backtab>") #'backward-button)

(keymap-set help-map "B" #'describe-keymap)
(keymap-set help-map "p" #'describe-package)
(keymap-set help-map "P" #'finder-by-keyword)

;;;; load

(defvar-keymap init-load-map
  "f" #'load-file
  "l" #'load-library
  "t" #'load-theme)

(keymap-set help-map "t" init-load-map)

(consult-customize consult-theme :preview-key '(:debounce 0.5 any))
(keymap-set init-consult-override-mode-map "<remap> <load-theme>" #'consult-theme)

;;;; findfunc

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

;;;; elookup

(defun init-describe-symbol-dwim ()
  "Describe symbol at point."
  (interactive)
  (describe-symbol (intern (init-thing-at-point-or-throw))))

(setq evil-lookup-func #'init-describe-symbol-dwim)

;;;; info

(require 'info)

(evil-set-initial-state 'Info-mode 'motion)

(evil-define-key 'motion Info-mode-map
  (kbd "RET") #'Info-follow-nearest-node
  (kbd "<return>") #'Info-follow-nearest-node
  (kbd "TAB") #'Info-next-reference
  (kbd "S-TAB") #'Info-prev-reference
  (kbd "<tab>") #'Info-next-reference
  (kbd "<backtab>") #'Info-prev-reference)

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

(evil-set-initial-state 'xref--xref-buffer-mode 'motion)

(evil-define-key 'motion xref--xref-buffer-mode-map
  (kbd "RET") #'xref-goto-xref
  (kbd "<return>") #'xref-goto-xref
  "go" #'xref-show-location-at-point
  "gj" #'xref-next-line
  "gk" #'xref-prev-line
  (kbd "C-j") #'xref-next-line
  (kbd "C-k") #'xref-prev-line
  "gr" #'xref-revert-buffer)

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

(defun init-around-company-capf-set-styles (func &rest args)
  "Set completion styles for `company-capf'.
FUNC ARGS see `company-capf'."
  (let ((completion-styles '(basic partial-completion)))
    (apply func args)))

(advice-add 'company-capf :around #'init-around-company-capf-set-styles)

(defun init-around-company-call-backend-do-check-evil (func command &rest args)
  "Check evil state before call company.
FUNC COMMAND ARGS see `company-call-backend'."
  (unless (and evil-mode (eq evil-state 'normal) (eq command 'prefix))
    (apply func command args)))

(advice-add #'company-call-backend :around #'init-around-company-call-backend-do-check-evil)

;;;; eglot

(require 'eglot)

(setq eglot-extend-to-xref t)

(keymap-set eglot-mode-map "<remap> <evil-lookup>" #'eldoc-doc-buffer)

;;; special

;;;; occur

(evil-set-initial-state 'occur-mode 'motion)

(evil-define-key 'motion occur-mode-map
  (kbd "RET") #'occur-mode-goto-occurrence
  (kbd "<return>") #'occur-mode-goto-occurrence
  (kbd "TAB") #'occur-next
  (kbd "S-TAB") #'occur-prev
  (kbd "<tab>") #'occur-next
  (kbd "<backtab>") #'occur-prev
  "go" #'occur-mode-display-occurrence
  "gj" #'next-error-no-select
  "gk" #'previous-error-no-select
  (kbd "C-j") #'next-error-no-select
  (kbd "C-k") #'previous-error-no-select)

;;;; dired

(require 'dired)
(require 'dired-x)

(setq dired-dwim-target t)
(setq dired-auto-revert-buffer t)
(setq dired-listing-switches "-lha")

(put 'dired-jump 'repeat-map nil)

(keymap-set ctl-x-4-map "j" #'dired-jump-other-window)

(evil-set-initial-state 'dired-mode 'motion)

(defun init-dired-next-line-dwim ()
  "Goto next line in Dired dwim."
  (interactive)
  (dired-next-line (if (bolp) 0 1)))

(evil-define-key 'motion dired-mode-map
  (kbd "RET") #'dired-find-file
  (kbd "<return>") #'dired-find-file
  (kbd "TAB") #'init-dired-next-line-dwim
  (kbd "S-TAB") #'dired-previous-line
  (kbd "<tab>") #'init-dired-next-line-dwim
  (kbd "<backtab>") #'dired-previous-line
  "go" #'dired-display-file
  "gj" #'dired-next-line
  "gk" #'dired-previous-line
  (kbd "C-j") #'dired-next-line
  (kbd "C-k") #'dired-previous-line
  "+" #'dired-create-directory)

;;;; archive

(require 'arc-mode)

(evil-set-initial-state 'archive-mode 'motion)

(defun init-archive-next-line-dwim ()
  "Goto next line in Archive dwim."
  (interactive)
  (archive-next-line (if (bolp) 0 1)))

(evil-define-key 'motion archive-mode-map
  (kbd "RET") #'archive-extract
  (kbd "<return>") #'archive-extract
  (kbd "TAB") #'init-archive-next-line-dwim
  (kbd "S-TAB") #'archive-previous-line
  (kbd "<tab>") #'init-archive-next-line-dwim
  (kbd "<backtab>") #'archive-previous-line
  "go" #'archive-display-other-window
  "gj" #'archive-next-line
  "gk" #'archive-previous-line
  (kbd "C-j") #'archive-next-line
  (kbd "C-k") #'archive-previous-line)

;;;; ibuffer

(require 'ibuffer)
(require 'ibuf-ext)

(setq ibuffer-formats
      '((mark modified read-only locked
              " " (name 40 40 :left :elide)
	      " " (size 9 -1 :right)
	      " " (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " " (name 40 -1) " " filename)))

(evil-set-initial-state 'ibuffer-mode 'motion)

(evil-define-key 'motion ibuffer-mode-map
  (kbd "RET") #'ibuffer-visit-buffer
  (kbd "<return>") #'ibuffer-visit-buffer
  (kbd "TAB") #'ibuffer-forward-line
  (kbd "S-TAB") #'ibuffer-backward-line
  (kbd "<tab>") #'ibuffer-forward-line
  (kbd "<backtab>") #'ibuffer-backward-line
  "gr" #'ibuffer-update
  "go" #'ibuffer-visit-buffer-other-window-noselect
  "gj" #'ibuffer-forward-filter-group
  "gk" #'ibuffer-backward-filter-group
  (kbd "C-j") #'ibuffer-forward-filter-group
  (kbd "C-k") #'ibuffer-backward-filter-group)

;;;; tabulated list

(require 'tabulated-list)

(evil-set-initial-state 'tabulated-list-mode 'motion)

(evil-define-key 'motion tabulated-list-mode-map
  (kbd "TAB") #'forward-button
  (kbd "S-TAB") #'backward-button
  (kbd "<tab>") #'forward-button
  (kbd "<backtab>") #'backward-button)

;;;; compile

(require 'compile)
(require 'ansi-color)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(evil-set-initial-state 'compilation-mode 'motion)

(evil-define-key 'motion compilation-mode-map
  (kbd "RET") #'compile-goto-error
  (kbd "<return>") #'compile-goto-error
  (kbd "TAB") #'compilation-next-error
  (kbd "S-TAB") #'compilation-previous-error
  (kbd "<tab>") #'compilation-next-error
  (kbd "<backtab>") #'compilation-previous-error
  "go" #'compilation-display-error
  "gj" #'next-error-no-select
  "gk" #'previous-error-no-select
  (kbd "C-j") #'next-error-no-select
  (kbd "C-k") #'previous-error-no-select)

;;;; grep

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

(evil-set-initial-state 'grep-mode 'motion)

(evil-define-key 'motion grep-mode-map
  (kbd "RET") #'compile-goto-error
  (kbd "<return>") #'compile-goto-error
  (kbd "TAB") #'compilation-next-error
  (kbd "S-TAB") #'compilation-previous-error
  (kbd "<tab>") #'compilation-next-error
  (kbd "<backtab>") #'compilation-previous-error
  "go" #'compilation-display-error
  "gj" #'next-error-no-select
  "gk" #'previous-error-no-select
  (kbd "C-j") #'next-error-no-select
  (kbd "C-k") #'previous-error-no-select)

;;;; diff

(require 'diff)

(evil-set-initial-state 'diff-mode 'motion)

(evil-define-key 'motion diff-mode-shared-map
  (kbd "RET") #'diff-goto-source
  (kbd "<return>") #'diff-goto-source
  "gj" #'diff-hunk-next
  "gk" #'diff-hunk-prev
  "gJ" #'diff-file-next
  "gK" #'diff-file-prev
  (kbd "C-j") #'diff-hunk-next
  (kbd "C-k") #'diff-hunk-prev)

;;;;; ediff

(require 'ediff)

(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;;;; logview

(require 'log-view)

(evil-set-initial-state 'log-view-mode 'motion)

(evil-define-key 'motion log-view-mode-map
  (kbd "TAB") #'log-view-toggle-entry-display
  (kbd "<tab>") #'log-view-toggle-entry-display
  (kbd "RET") #'log-view-toggle-entry-display
  (kbd "<return>") #'log-view-toggle-entry-display
  "gj" #'log-view-msg-next
  "gk" #'log-view-msg-prev
  (kbd "C-j") #'log-view-msg-next
  (kbd "C-k") #'log-view-msg-prev)

;;;; vc

(require 'vc-dir)
(require 'vc-annotate)

(keymap-set vc-prefix-map "p" #'vc-push)

(evil-set-initial-state 'vc-dir-mode 'motion)
(evil-set-initial-state 'vc-annotate-mode 'motion)
(evil-set-initial-state 'vc-git-log-view-mode 'motion)

(evil-define-key 'motion vc-dir-mode-map
  (kbd "RET") #'vc-dir-find-file
  (kbd "<return>") #'vc-dir-find-file
  "go" #'vc-dir-display-file
  "gj" #'vc-dir-next-line
  "gk" #'vc-dir-previous-line
  (kbd "C-j") #'vc-dir-next-line
  (kbd "C-k") #'vc-dir-previous-line
  "p" #'vc-push)

(evil-define-key 'motion vc-annotate-mode-map
  (kbd "RET") #'vc-annotate-goto-line
  (kbd "<return>") #'vc-annotate-goto-line
  "gj" #'vc-annotate-next-revision
  "gk" #'vc-annotate-prev-revision
  (kbd "C-j") #'vc-annotate-next-revision
  (kbd "C-k") #'vc-annotate-prev-revision)

;;;; comint

(require 'comint)

(evil-define-key 'insert comint-mode-map
  (kbd "M-r") #'comint-history-isearch-backward-regexp)

(evil-define-key 'normal comint-mode-map
  (kbd "RET") #'comint-send-input
  (kbd "<return>") #'comint-send-input
  "gj" #'comint-next-prompt
  "gk" #'comint-previous-prompt
  (kbd "C-j") #'comint-next-prompt
  (kbd "C-k") #'comint-previous-prompt)

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

(evil-define-key 'insert eshell-hist-mode-map
  (kbd "M-r") #'eshell-previous-matching-input)

(evil-define-key 'normal eshell-mode-map
  (kbd "RET") #'eshell-send-input
  (kbd "<return>") #'eshell-send-input
  "gj" #'eshell-next-prompt
  "gk" #'eshell-previous-prompt
  (kbd "C-j") #'eshell-next-prompt
  (kbd "C-k") #'eshell-previous-prompt)

(defun init-eshell-dwim-find-buffer ()
  "Find eshell dwim buffer."
  (->> (buffer-list)
       (--first
        (and (eq (buffer-local-value 'major-mode it) 'eshell-mode)
             (string-prefix-p eshell-buffer-name (buffer-name it))
             (not (get-buffer-process it))
             (not (get-buffer-window it))))))

(defun init-eshell-dwim-get-buffer-create ()
  "Get eshell dwim buffer, create if not exist."
  (-if-let (buffer (init-eshell-dwim-find-buffer))
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

;;;; custom

(require 'cus-edit)

(evil-set-initial-state 'Custom-mode 'motion)

(evil-define-key 'motion custom-mode-map
  (kbd "RET") #'Custom-newline
  (kbd "<return>") #'Custom-newline
  (kbd "TAB") #'widget-forward
  (kbd "S-TAB") #'widget-backward
  (kbd "<tab>") #'widget-forward
  (kbd "<backtab>") #'widget-backward
  "q" #'Custom-buffer-done)

;;;; package

(require 'package)

(evil-set-initial-state 'package-menu-mode 'motion)

;;;; spell

(require 'ispell)

(setq ispell-dictionary "american")

;;;; magit section

(require 'magit-section)

(evil-set-initial-state 'magit-section-mode 'motion)

(evil-define-key 'motion magit-section-mode-map
  (kbd "TAB") #'magit-section-toggle
  (kbd "S-TAB") #'magit-section-cycle-global
  (kbd "<tab>") #'magit-section-toggle
  (kbd "<backtab>") #'magit-section-cycle-global
  "gj" #'magit-section-forward-sibling
  "gk" #'magit-section-backward-sibling
  (kbd "C-j") #'magit-section-forward-sibling
  (kbd "C-k") #'magit-section-backward-sibling)

;;;; magit

(require 'magit)

(defun init-magit-dwim (&optional arg)
  "Call magit dwim.
If in `magit-mode' derived mode, or with more than 2 universal ARG,
or with universal arg and not in a file buffer, call `magit-dispatch';
If with universal arg and in a file buffer, call `magit-file-dispatch';
Or else call `magit-status'."
  (interactive "P")
  (let ((command (cond ((or
                         ;; in a `magit-mode' derived mode
                         (derived-mode-p 'magit-mode)
                         ;; with more than 2 universal arg
                         (> (prefix-numeric-value arg) 4)
                         ;; with universal arg and not in a file
                         (and arg (not buffer-file-name)))
                        #'magit-dispatch)
                       ;; with universal arg and in a file
                       ((and arg buffer-file-name)
                        #'magit-file-dispatch)
                       (t
                        #'magit-status))))
    (setq this-command command)
    (call-interactively this-command)))

(evil-set-initial-state 'magit-mode 'motion)
(evil-set-initial-state 'magit-status-mode 'motion)
(evil-set-initial-state 'magit-diff-mode 'motion)
(evil-set-initial-state 'magit-log-mode 'motion)
(evil-set-initial-state 'magit-revision-mode 'motion)
(evil-set-initial-state 'magit-stash-mode 'motion)
(evil-set-initial-state 'magit-stashes-mode 'motion)
(evil-set-initial-state 'magit-process-mode 'motion)

(evil-define-key 'motion magit-mode-map
  (kbd "RET") #'magit-visit-thing
  (kbd "<return>") #'magit-visit-thing
  "q" #'magit-mode-bury-buffer
  "j" #'magit-next-line
  "k" #'magit-previous-line
  "gr" #'magit-refresh
  "gR" #'magit-refresh-all
  "p" #'magit-push)

(evil-define-key 'visual magit-mode-map
  "j" #'evil-next-line
  "k" #'evil-previous-line)

(evil-define-minor-mode-key 'motion 'magit-blob-mode
  "q" #'magit-kill-this-buffer
  "gj" #'magit-blob-next
  "gk" #'magit-blob-previous
  (kbd "C-j") #'magit-blob-next
  (kbd "C-k") #'magit-blob-previous)

(evil-define-minor-mode-key 'motion 'magit-blame-mode
  "q" #'magit-blame-quit
  "gj" #'magit-blame-next-chunk
  "gk" #'magit-blame-previous-chunk
  "gJ" #'magit-blame-next-chunk-same-commit
  "gK" #'magit-blame-previous-chunk-same-commit
  (kbd "C-j") #'magit-blame-next-chunk
  (kbd "C-k") #'magit-blame-previous-chunk)

;;; leaders

(defun init-leader-bindings (clauses)
  "Transforms `define-key' CLAUSES to binding alist."
  (->> clauses
       (-partition 2)
       (--map
        (cons (kbd (concat "SPC " (car it))) (cadr it)))))

(defun init-leader-set (keymap &rest clauses)
  "Define leader binding CLAUSES in KEYMAP."
  (declare (indent defun))
  (dolist (binding (init-leader-bindings clauses))
    (evil-define-key* 'motion keymap (car binding) (cdr binding))))

(defun init-leader-global-set (&rest clauses)
  "Define leader binding CLAUSES in `init-evil-override-mode-map'."
  (apply #'init-leader-set init-evil-override-mode-map clauses))

(defun init-magic (prefix)
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
               (setq this-command binding
                     real-this-command binding)
               (if (commandp binding t)
                   (call-interactively binding)
                 (execute-kbd-macro binding)))
              ((and binding (keymapp binding))
               (init-magic new-prefix))
              (t
               (user-error "No magic key binding found on %s %c" prefix char)))))))

(defun init-magic-C-c ()
  "Magic control C."
  (interactive)
  (init-magic "C-c"))

(defun init-magic-C-u ()
  "Magic control U."
  (interactive)
  (setq prefix-arg
        (list (if current-prefix-arg
                  (* 4 (prefix-numeric-value current-prefix-arg))
                4)))
  (set-transient-map (key-binding " ")))

(defvar init-magic-shift-special
  '((?1 . ?!) (?2 . ?@) (?3 . ?#) (?4 . ?$) (?5 . ?%) (?6 . ?^) (?7 . ?&) (?8 . ?*) (?9 . ?\() (?0 . ?\))
    (?- . ?_) (?= . ?+) (?` . ?~) (?\[ . ?\{) (?\] . ?\}) (?\\ . ?|) (?, . ?<) (?. . ?>) (?/ . ??)))

(defun init-magic-shift ()
  "Magic shift."
  (interactive)
  (let* ((event (read-event))
         (shift-event (or (cdr (assq event init-magic-shift-special)) (upcase event))))
    (setq this-command #'ignore)
    (setq real-this-command #'ignore)
    (setq prefix-arg current-prefix-arg)
    (push shift-event unread-command-events)
    (push 32 unread-command-events)))

(defvar-keymap init-minor-prefix-map
  "s" #'auto-save-visited-mode
  "r" #'global-auto-revert-mode
  "t" #'toggle-truncate-lines
  "v" #'visual-line-mode
  "w" #'whitespace-mode
  "W" #'whitespace-newline-mode
  "h" #'hl-line-mode
  "n" #'display-line-numbers-mode
  "N" #'init-toggle-line-numbers-relative)

(init-leader-global-set
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
 "e" #'init-eshell-dwim
 "G" #'init-magit-dwim
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

;;; lang

;;;; lisp

(defun init-lisp-outline-level ()
  "Return level of current outline heading."
  (if (looking-at ";;\\([;*]+\\)")
      (- (match-end 1) (match-beginning 1))
    (funcall outline-level)))

(defun init-lisp-set-outline ()
  "Set outline vars."
  (setq-local outline-regexp ";;[;*]+[\s\t]+")
  (setq-local outline-level #'init-lisp-outline-level))

(defun init-lisp-around-last-sexp-maybe-forward (func &rest args)
  "Around *-last-sexp command.
Save point and forward sexp before command if looking at an open paren.
FUNC and ARGS see specific command."
  (save-excursion
    (when (looking-at-p "(\\|\\[\\|{")
      (forward-sexp))
    (apply func args)))

;;;; elisp

(defvar init-elisp-modes
  '(emacs-lisp-mode lisp-interaction-mode))

(defvar init-elisp-hooks
  '(emacs-lisp-mode-hook lisp-interaction-mode-hook))

(defvar init-elisp-last-sexp-commands
  (list #'eval-last-sexp
        #'eval-print-last-sexp
        #'pp-eval-last-sexp
        #'pp-macroexpand-last-sexp))

(dolist (command init-elisp-last-sexp-commands)
  (advice-add command :around #'init-lisp-around-last-sexp-maybe-forward))

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (keymap-set map "C-c C-k" #'eval-buffer)
  (keymap-set map "C-C C-l" #'load-file)
  (keymap-set map "C-c C-m" #'pp-macroexpand-last-sexp))

(dolist (hook init-elisp-hooks)
  (add-hook hook #'init-lisp-set-outline))

(dolist (mode init-elisp-modes)
  (add-to-list 'init-evil-eval-function-alist `(,mode . eval-region)))

;;;;; ielm

(require 'ielm)

(defun init-ielm-other-window ()
  "Switch to elisp repl other window."
  (interactive)
  (pop-to-buffer (get-buffer-create "*ielm*"))
  (ielm))

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (keymap-set map "C-c C-z" #'init-ielm-other-window))

(evil-define-key 'normal ielm-map
  (kbd "RET") #'ielm-return
  (kbd "<return>") #'ielm-return)

;;;;; flymake

(setq trusted-content (list (file-name-as-directory (abbreviate-file-name init-lisp-directory))))

(setq elisp-flymake-byte-compile-load-path load-path)

(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

;;;;; dash

(dash-register-info-lookup)

(global-dash-fontify-mode 1)

;;;; outline

(require 'outline)

(evil-define-key 'motion outline-mode-map
  (kbd "TAB") #'outline-toggle-children
  (kbd "S-TAB") #'outline-show-all
  (kbd "<tab>") #'outline-toggle-children
  (kbd "<backtab>") #'outline-show-all
  "gj" #'outline-forward-same-level
  "gk" #'outline-backward-same-level
  (kbd "C-j") #'outline-forward-same-level
  (kbd "C-k") #'outline-backward-same-level)

;;;; org

(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'embark-org)

(add-to-list 'org-modules 'org-tempo)

(setq org-special-ctrl-a/e t)

(keymap-global-set "C-c l" #'org-insert-link-global)

(init-leader-global-set
 "C" #'org-capture
 "A" #'org-agenda
 "W" #'org-store-link
 "O" #'org-open-at-point-global)

(defun init-org-set-syntax ()
  "Modify `org-mode' syntax table."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(add-hook 'org-mode-hook #'init-org-set-syntax)

(keymap-set org-mode-map "C-c C-'" #'org-edit-special)
(keymap-set org-src-mode-map "C-c C-'" #'org-edit-src-exit)
(keymap-set org-src-mode-map "C-c C-c" #'org-edit-src-exit)

(init-leader-set org-mode-map
  "n b" #'org-narrow-to-block
  "n s" #'org-narrow-to-subtree)

(evil-define-key 'motion org-mode-map
  (kbd "TAB") #'org-cycle
  (kbd "S-TAB") #'org-shifttab
  (kbd "<tab>") #'org-cycle
  (kbd "<backtab>") #'org-shifttab)

(defun init-around-org-make-tag-string-do-sort (func tags)
  "Do sort org tags before make string.
FUNC TAGS see `org-make-tag-string'."
  (funcall func (sort tags)))

(advice-add #'org-make-tag-string :around #'init-around-org-make-tag-string-do-sort)

(defun init-org-echo-link ()
  "Echo org link in minibuffer."
  (interactive)
  (when (org-in-regexp org-link-any-re)
    (let (message-log-max)
      (message (match-string-no-properties 0)))))

(keymap-set embark-org-link-map "e" #'init-org-echo-link)

;;;;; agenda

(setq org-directory (expand-file-name "org" user-emacs-directory))
(setq org-agenda-files (list org-directory))
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

(setq org-capture-templates
      '(("t" "Task" entry (file+headline "" "Tasks") "* TODO %?\n%U\n%a")))

(evil-set-initial-state 'org-agenda-mode 'motion)

(evil-define-key 'motion org-agenda-mode-map
  (kbd "RET") #'org-agenda-goto
  (kbd "<return>") #'org-agenda-goto
  "gr" #'org-agenda-redo
  "gR" #'org-agenda-redo-all
  "go" #'org-agenda-show
  "j" #'org-agenda-next-line
  "k" #'org-agenda-previous-line)

;;;; markdown

(require 'markdown-mode)
(require 'edit-indirect)

(setq markdown-special-ctrl-a/e t)
(setq markdown-fontify-code-blocks-natively t)

(keymap-set markdown-mode-map "C-c C-'" #'markdown-edit-code-block)
(keymap-set edit-indirect-mode-map "C-c C-'" #'edit-indirect-commit)

(init-leader-set markdown-mode-map
  "n b" #'markdown-narrow-to-block
  "n s" #'markdown-narrow-to-subtree)

(evil-define-key 'motion markdown-mode-map
  (kbd "TAB") #'markdown-cycle
  (kbd "S-TAB") #'markdown-shifttab
  (kbd "<tab>") #'markdown-cycle
  (kbd "<backtab>") #'markdown-shifttab
  "gj" #'markdown-outline-next-same-level
  "gk" #'markdown-outline-previous-same-level
  (kbd "C-j") #'markdown-outline-next-same-level
  (kbd "C-k") #'markdown-outline-previous-same-level)

;;;; web

(require 'sgml-mode)
(require 'css-mode)
(require 'js)

(setq css-indent-offset 2)
(setq js-indent-level 2)

;;;; python

(require 'python)

(defvar init-python-modes
  '(python-mode python-ts-mode inferior-python-mode))

(defvar init-python-hooks
  '(python-base-mode-hook inferior-python-mode-hook))

(setq python-shell-interpreter "python")
(setq python-shell-interpreter-args "-m IPython --simple-prompt")

(keymap-set python-base-mode-map "C-c C-k" #'python-shell-send-buffer)

(dolist (mode init-python-modes)
  (add-to-list 'init-evil-eval-function-alist `(,mode . python-shell-send-region)))

;;; end

(provide 'init-emacs)
;;; init-emacs.el ends here
