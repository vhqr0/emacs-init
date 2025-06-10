;;; init-emacs.el --- Init Emacs -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Emacs itself.

(require 'dash)
(require 's)
(require 'f)
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



;;; files

(require 'files)
(require 'vc-hooks)
(require 'vc-git)
(require 'project)

(setq project-mode-line t)
(setq vc-handled-backends '(Git))
(setq vc-make-backup-files t)
(setq version-control t)
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq delete-by-moving-to-trash t)

(setq auto-save-file-name-transforms `((".*" ,(f-expand "save/" user-emacs-directory) t)))
(setq lock-file-name-transforms      `((".*" ,(f-expand "lock/" user-emacs-directory) t)))
(setq backup-directory-alist         `((".*" . ,(f-expand "backup/" user-emacs-directory))))

;;;; auto save visited

(setq auto-save-visited-interval 0.5)

(add-to-list 'minor-mode-alist '(auto-save-visited-mode " ASave"))

(defvar init-disable-auto-save-visited-predicates nil)

(defun init-auto-save-visited-predicate ()
  "Predication of `auto-save-visited-mode'."
  (not (run-hook-with-args-until-success 'init-disable-auto-save-visited-predicates)))

(setq auto-save-visited-predicate #'init-auto-save-visited-predicate)

(auto-save-visited-mode 1)

;;;; recentf

(require 'recentf)

(setq recentf-max-saved-items 500)

(recentf-mode 1)

(keymap-set ctl-x-r-map "e" #'recentf-open)

;;;; saveplace

(require 'saveplace)

(save-place-mode 1)

;;;; so-long

(require 'so-long)

(global-so-long-mode 1)

;;;; revert

(keymap-set ctl-x-x-map "G" #'revert-buffer)
(keymap-set ctl-x-x-map "v" #'vc-refresh-state)

(keymap-set goto-map "r" #'revert-buffer-quick)
(keymap-set goto-map "R" #'revert-buffer)



;;; ui

;;;; graphic elements

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

(setq use-dialog-box nil)
(setq use-file-dialog nil)

(setq ring-bell-function #'ignore)

(defvar init-disabled-ui-modes
  '(blink-cursor-mode tooltip-mode tool-bar-mode menu-bar-mode scroll-bar-mode))

(defun init-disable-ui ()
  "Disable various ui modes."
  (interactive)
  (dolist (mode init-disabled-ui-modes)
    (when (fboundp mode)
      (funcall mode -1))))

(init-disable-ui)

;;;; windows

(require 'windmove)

(windmove-default-keybindings)

(require 'tab-bar)

(setq tab-bar-tab-hints t)
(setq tab-bar-select-tab-modifiers '(meta))
(setq tab-bar-close-last-tab-choice 'delete-frame)

(tab-bar-mode 1)
(tab-bar-history-mode 1)

(defvar-keymap init-tab-bar-history-repeat-map
  :repeat t
  "<left>" #'tab-bar-history-back
  "<right>" #'tab-bar-history-forward)

(keymap-global-set "C-S-N" #'make-frame-command)
(keymap-global-set "C-S-T" #'tab-bar-new-tab)
(keymap-global-set "C-S-W" #'tab-bar-close-tab)

(keymap-global-set "C-0" #'global-text-scale-adjust)
(keymap-global-set "C--" #'global-text-scale-adjust)
(keymap-global-set "C-=" #'global-text-scale-adjust)

(keymap-global-set "C-M-0" #'text-scale-adjust)
(keymap-global-set "C-M--" #'text-scale-decrease)
(keymap-global-set "C-M-=" #'text-scale-increase)



;;; edit

;;;; commands

(setq disabled-command-function nil)
(setq suggest-key-bindings nil)

(setq enable-recursive-minibuffers t)

(keymap-global-set "C-SPC" #'toggle-input-method)

(require 'repeat)

(repeat-mode 1)

(require 'embark)

(keymap-global-set "M-o" #'embark-act)
(keymap-global-set "M-O" #'embark-act-all)

;;;; indent

(setq-default indent-tabs-mode nil)

(setq-default truncate-lines t)

(setq word-wrap-by-category t)

;;;; parens

(require 'elec-pair)

(electric-pair-mode 1)

(require 'paren)

(setq show-paren-style 'expression)
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

(defun init-wrap-pair (&optional arg)
  "Insert pair, ARG see `insert-pair'."
  (interactive "P")
  (insert-pair (or arg 1)))

(defun init-wrap-pair-common (&optional arg)
  "Insert pair (, ARG see `init-wrap-pair'."
  (interactive "P")
  (let ((last-command-event ?\())
    (init-wrap-pair arg)))

;;;; iedit

(require 'iedit)

;;;; visual

(defun init-set-trailing-whitespace-display ()
  "Set local display of trailing whitespace."
  (setq-local show-trailing-whitespace t))

(add-hook 'text-mode-hook #'init-set-trailing-whitespace-display)
(add-hook 'prog-mode-hook #'init-set-trailing-whitespace-display)

(require 'display-line-numbers)

(defun init-toggle-line-numbers-type ()
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

;;;; keyboard

(defun init-capslock-event (event)
  "Convert capslock last command EVENT."
  (if (numberp event) (upcase event) event))

(defun init-capslock-self-insert ()
  "Self insert with upcase lower case char."
  (interactive)
  (let ((last-command-event (init-capslock-event last-command-event)))
    (setq this-command #'self-insert-command)
    (call-interactively #'self-insert-command)))

(defvar-keymap init-capslock-mode-map)

(define-key init-capslock-mode-map [remap self-insert-command] #'init-capslock-self-insert)

(define-minor-mode init-capslock-mode
  "Upcase lower case key."
  :group 'init
  :global t
  :lighter " Capslock"
  :keymap init-capslock-mode-map)

(defvar init-qwerty-prog-bindings
  '((?1 . ?!) (?2 . ?@) (?3 . ?#) (?4 . ?$) (?5 . ?%) (?6 . ?^) (?7 . ?&) (?8 . ?*) (?9 . ?\() (?0 . ?\))
    (?! . ?1) (?@ . ?2) (?# . ?3) (?$ . ?4) (?% . ?5) (?^ . ?6) (?& . ?7) (?* . ?8) (?\( . ?9) (?\) . ?0)))

(defun init-qwerty-prog-event (event)
  "Convert qwerty prog last command EVENT."
  (let ((binding (assq event init-qwerty-prog-bindings)))
    (or (cdr binding) event)))

(defun init-qwerty-prog-self-insert ()
  "Self insert with qwerty prog keyboard translation."
  (interactive)
  (let ((last-command-event (init-qwerty-prog-event last-command-event)))
    (setq this-command #'self-insert-command)
    (call-interactively #'self-insert-command)))

(defvar-keymap init-qwerty-prog-mode-map)

(dolist (binding init-qwerty-prog-bindings)
  (define-key init-qwerty-prog-mode-map (vector (car binding)) #'init-qwerty-prog-self-insert))

(define-minor-mode init-qwerty-prog-mode
  "Invert number key in qwerty keyboard."
  :group 'init
  :global t
  :lighter " QwertyProg"
  :keymap init-qwerty-prog-mode-map)

(init-qwerty-prog-mode 1)



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

(defvar evil-search-module)
(setq evil-search-module 'evil-search)

(defvar evil-undo-system)
(setq evil-undo-system 'undo-redo)

(require 'evil)

(setq evil-want-fine-undo t)
(setq evil-search-wrap nil)
(setq evil-symbol-word-search t)

(evil-mode 1)

(defvar init-evil-adjust-cursor-disabled-commands
  '(forward-sexp forward-list))

(defun init-around-evil-adjust-cursor (func &rest args)
  "Dont adjust cursor after certain commands.
FUNC and ARGS see `evil-set-cursor'."
  (unless (memq this-command init-evil-adjust-cursor-disabled-commands)
    (apply func args)))

(advice-add #'evil-adjust-cursor :around #'init-around-evil-adjust-cursor)

(define-key evil-normal-state-map [remap yank-pop] nil t)

(keymap-unset evil-insert-state-map "C-@" t)
(keymap-unset evil-insert-state-map "C-a" t)
(keymap-unset evil-insert-state-map "C-k" t)
(keymap-unset evil-insert-state-map "C-w" t)

(keymap-set evil-motion-state-map "-" #'negative-argument)
(keymap-set evil-motion-state-map "C-q" #'evil-record-macro)
(keymap-set evil-motion-state-map "q" #'quit-window)
(keymap-set evil-normal-state-map "q" #'quit-window)

(keymap-set evil-visual-state-map "m" #'evil-jump-item)
(keymap-set evil-operator-state-map "m" #'evil-jump-item)
(keymap-set evil-operator-state-map "o" #'evil-inner-symbol)
(keymap-set evil-operator-state-map "p" #'evil-inner-paragraph)

(keymap-set evil-normal-state-map "M-r" #'raise-sexp)
(keymap-set evil-normal-state-map "M-s" #'paredit-splice-sexp)

(set-keymap-parent evil-command-line-map minibuffer-local-map)

(add-to-list 'init-disable-auto-save-visited-predicates #'evil-insert-state-p)

(keymap-set evil-window-map "<left>" #'tab-bar-history-back)
(keymap-set evil-window-map "<right>" #'tab-bar-history-forward)

;;;; evil collection

;; must be set before evil collection loaded.

(defvar evil-collection-setup-minibuffer)
(setq evil-collection-setup-minibuffer t)

(defvar evil-collection-calendar-want-org-bindings)
(setq evil-collection-calendar-want-org-bindings t)

(require 'evil-collection)

(evil-collection-init)

(init-diminish-minor-mode 'evil-collection-unimpaired-mode)

;;;; evil surround

(require 'evil-surround)

(add-to-list 'evil-surround-pairs-alist '(?r . ("[" . "]")))
(add-to-list 'evil-surround-pairs-alist '(?a . ("<" . ">")))
(add-to-list 'evil-surround-pairs-alist '(?# . ("#{" . "}")))

(keymap-set evil-inner-text-objects-map "r" #'evil-inner-bracket)
(keymap-set evil-outer-text-objects-map "r" #'evil-a-bracket)
(keymap-set evil-inner-text-objects-map "a" #'evil-inner-angle)
(keymap-set evil-outer-text-objects-map "a" #'evil-an-angle)

(global-evil-surround-mode 1)

;;;; evil extra

(defun init-evil-escape ()
  ":imap jk <esc>."
  (interactive)
  (if (or executing-kbd-macro
          defining-kbd-macro
          (sit-for 0.15 t))
      (insert (if init-capslock-mode ?J ?j))
    (let ((event (read-event)))
      (if (= event ?k)
          (progn
            (setq this-command #'ignore
                  real-this-command #'ignore)
            (push 'escape unread-command-events))
        (insert (if init-capslock-mode ?J ?j))
        (push event unread-command-events)))))

(evil-define-operator init-evil-operator-comment (beg end)
  :move-point nil
  (interactive "<r>")
  (comment-or-uncomment-region beg end))

(evil-define-operator init-evil-operator-narrow (beg end)
  :move-point nil
  (interactive "<r>")
  (narrow-to-region beg end))

(defvar init-evil-eval-function-alist
  '((emacs-lisp-mode . eval-region)
    (lisp-interaction-mode . eval-region)))

(evil-define-operator init-evil-operator-eval (beg end)
  :move-point nil
  (interactive "<r>")
  (-when-let (eval-function (cdr (assq major-mode init-evil-eval-function-alist)))
    (funcall eval-function beg end)))

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

(keymap-set evil-insert-state-map "j" #'init-evil-escape)
(keymap-set evil-replace-state-map "j" #'init-evil-escape)
(keymap-set evil-normal-state-map "g c" #'init-evil-operator-comment)
(keymap-set evil-motion-state-map "g -" #'init-evil-operator-narrow)
(keymap-set evil-motion-state-map "g y" #'init-evil-operator-eval)
(keymap-set evil-inner-text-objects-map "l" #'init-evil-inner-line)
(keymap-set evil-outer-text-objects-map "l" #'init-evil-a-line)
(keymap-set evil-inner-text-objects-map "d" #'init-evil-inner-defun)
(keymap-set evil-outer-text-objects-map "d" #'init-evil-a-defun)
(keymap-set evil-inner-text-objects-map "h" #'init-evil-text-object-entire)
(keymap-set evil-outer-text-objects-map "h" #'init-evil-text-object-entire)

(defvar-keymap init-evil-override-mode-map)

(define-minor-mode init-evil-override-mode
  "Override leader prefix map."
  :group 'init
  :global t
  :keymap init-evil-override-mode-map)

(init-evil-override-mode 1)



;;; goggles

(defvar init-goggles-changes nil)

(defun init-goggles-post-command ()
  "Highlight changes post command."
  (when init-goggles-changes
    (let ((start most-positive-fixnum)
          (end 0))
      (dolist (change init-goggles-changes)
        (when (eq (current-buffer) (marker-buffer (car change)))
          (setq start (min start (car change)))
          (setq end (max end (cdr change)))
          (set-marker (car change) nil)
          (set-marker (cdr change) nil)))
      (pulse-momentary-highlight-region start end)
      (setq init-goggles-changes nil))))

(defun init-goggles-after-change (start end len)
  "Push change to history.
START END LEN see `after-change-functions'."
  (when (and (/= len 0) (= start end))
    (when (> start (buffer-size))
      (setq start (- start 1)))
    (setq end (1+ start)))
  (let ((change (cons (copy-marker start) (copy-marker end))))
    (push change init-goggles-changes)))

(define-minor-mode init-goggles-mode
  "Init goggles mode."
  :lighter nil
  (if init-goggles-mode
      (progn
        (add-hook 'post-command-hook #'init-goggles-post-command nil t)
        (add-hook 'after-change-functions #'init-goggles-after-change nil t))
    (remove-hook 'post-command-hook #'init-goggles-post-command t)
    (remove-hook 'after-change-functions #'init-goggles-after-change t)))

(add-hook 'prog-mode-hook #'init-goggles-mode)
(add-hook 'text-mode-hook #'init-goggles-mode)
(add-hook 'minibuffer-mode-hook #'init-goggles-mode)



;;; completion

(define-key minibuffer-local-map [remap quit-window] #'abort-recursive-edit)

(evil-define-key 'insert minibuffer-mode-map
  (kbd "M-r") #'previous-matching-history-element)

;;;; savehist

(require 'savehist)

(savehist-mode 1)

;;;; isearch

(setq isearch-lazy-count t)
(setq isearch-allow-scroll t)
(setq isearch-allow-motion t)
(setq isearch-yank-on-move t)
(setq isearch-motion-changes-direction t)
(setq isearch-repeat-on-direction-change t)

(keymap-set embark-identifier-map "%" #'query-replace)
(keymap-set embark-general-map "C-M-s" #'embark-isearch-forward)
(keymap-set embark-general-map "C-M-r" #'embark-isearch-backward)

;;;; completion style

(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-extended-command-predicate #'command-completion-default-include-p)

(require 'orderless)

(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides nil)

(require 'marginalia)

(marginalia-mode 1)

;;;; vertico

(require 'vertico)
(require 'vertico-multiform)
(require 'vertico-repeat)
(require 'vertico-suspend)
(require 'vertico-directory)

(setq vertico-resize nil)

(vertico-mode 1)
(vertico-multiform-mode 1)

(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

(keymap-global-set "C-c b" #'vertico-repeat)
(keymap-global-set "C-c z" #'vertico-suspend)

(keymap-set vertico-map "C-l" #'vertico-directory-up)

(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(defvar init-vertico-disabled-commands '(kill-buffer))

(defun init-around-vertico-setup (func &rest args)
  "Disable vertico around `init-vertico-disabled-commands'.
FUNC ARGS see `vertico--setup'."
  (unless (memq this-command init-vertico-disabled-commands)
    (apply func args)))

(advice-add 'vertico--setup :around #'init-around-vertico-setup)

(defun init-vertico-embark-preview ()
  "Previews candidate in vertico buffer."
  (interactive)
  (save-selected-window
    (let ((embark-quit-after-action nil))
      (embark-dwim))))

(keymap-set vertico-map "C-j" #'init-vertico-embark-preview)
(keymap-set vertico-map "C-x C-s" #'embark-export)

;;;; consult

(require 'consult)
(require 'embark-consult)

(setq consult-preview-key '(:debounce 0.3 any))

(setq completion-in-region-function #'consult-completion-in-region)

(defvar-keymap init-consult-override-mode-map)

(define-minor-mode init-consult-override-mode
  "Override consult commands."
  :group 'init
  :global t
  :keymap init-consult-override-mode-map)

(init-consult-override-mode 1)

;;;;; history

(define-key init-consult-override-mode-map [remap yank] #'consult-yank-from-kill-ring)
(define-key init-consult-override-mode-map [remap yank-pop] #'consult-yank-pop)
(define-key init-consult-override-mode-map [remap previous-matching-history-element] #'consult-history)
(define-key init-consult-override-mode-map [remap eshell-previous-matching-input] #'consult-history)
(define-key init-consult-override-mode-map [remap comint-history-isearch-backward-regexp] #'consult-history)

;;;;; search

(require 'consult-imenu)

(setq consult-line-start-from-top t)

(consult-customize
 consult-goto-line
 consult-imenu
 consult-line
 :preview-key 'any)

(define-key init-consult-override-mode-map [remap goto-line] #'consult-goto-line)
(define-key init-consult-override-mode-map [remap imenu] #'consult-imenu)

(defun init-set-search-after-consult-line (&rest _args)
  "Set `evil-ex-search-pattern' after `consult-line'."
  (let ((pattern (car consult--line-history)))
    (setq evil-ex-search-pattern (list pattern t t))
    (evil-ex-nohighlight)))

(advice-add #'consult-line :after #'init-set-search-after-consult-line)

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
          (let ((name (concat
                       (propertize (format "%5d " line) 'face 'consult-line-number)
                       (s-join (propertize " / " 'face 'consult-line-number) (reverse stack)))))
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
(define-key init-consult-override-mode-map [remap load-theme] #'consult-theme)

;;;; find func

(require 'find-func)

;; TODO replace with `find-function-mode' after 30.1
;; (find-function-mode 1)
(find-function-setup-keys)

(keymap-set help-map "L" #'find-library)
(keymap-set help-map "F" #'find-function)
(keymap-set help-map "V" #'find-variable)
(keymap-set help-map "4 L" #'find-library-other-window)
(keymap-set help-map "4 F" #'find-function-other-window)
(keymap-set help-map "4 V" #'find-variable-other-window)
(keymap-set help-map "5 L" #'find-library-other-frame)
(keymap-set help-map "5 F" #'find-function-other-frame)
(keymap-set help-map "5 V" #'find-variable-other-frame)

;;;; elisp lookup

(defun init-describe-symbol-dwim ()
  "Describe symbol at point."
  (interactive)
  (describe-symbol (intern (init-thing-at-point-or-throw))))

(setq evil-lookup-func #'init-describe-symbol-dwim)



;;; prog

;;;; compile

(require 'compile)
(require 'ansi-color)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;;;; flymake

(require 'flymake)
(require 'flymake-proc)

(remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)

(setq flymake-no-changes-timeout 1)
(setq flymake-show-diagnostics-at-end-of-line 'short)

(keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
(keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error)

(defvar-local init-flymake-find-executable-function nil)
(defvar-local init-flymake-make-command-function nil)
(defvar-local init-flymake-report-diags-function nil)

(defvar-local init-flymake-proc nil)

(defun init-flymake-make-proc (buffer report-fn)
  "Make Flymake process for BUFFER.
REPORT-FN see `init-flymake-backend'."
  (-when-let (find-executable-fn (buffer-local-value 'init-flymake-find-executable-function buffer))
    (when (funcall find-executable-fn buffer)
      (let* ((proc-buffer-name (format "*init-flymake for %s*" (buffer-name buffer)))
             (make-command-fn (buffer-local-value 'init-flymake-make-command-function buffer))
             (report-diags-fn (buffer-local-value 'init-flymake-report-diags-function buffer))
             (command (funcall make-command-fn buffer))
             (sentinel
              (lambda (proc _event)
                (when (memq (process-status proc) '(exit signal))
                  (let ((proc-buffer (process-buffer proc)))
                    (unwind-protect
                        (if (eq proc (buffer-local-value 'init-flymake-proc buffer))
                            (funcall report-diags-fn buffer proc-buffer report-fn)
                          (flymake-log :warning "Canceling obsolete checker %s" proc))
                      (kill-buffer proc-buffer)))))))
        (make-process
         :name proc-buffer-name
         :noquery t
         :connection-type 'pipe
         :buffer (generate-new-buffer-name proc-buffer-name)
         :command command
         :sentinel sentinel)))))

(defun init-flymake-backend (report-fn &rest _args)
  "Generic Flymake backend.
REPORT-FN see `flymake-diagnostic-functions'."
  (-when-let (proc (init-flymake-make-proc (current-buffer) report-fn))
    (when (process-live-p init-flymake-proc)
      (kill-process init-flymake-proc))
    (setq init-flymake-proc proc)
    (save-restriction
      (widen)
      (process-send-region proc (point-min) (point-max))
      (process-send-eof proc))))

;;;; eldoc

(require 'eldoc)

(setq eldoc-minor-mode-string nil)

;;;; xref

(require 'xref)

(setq xref-search-program 'ripgrep)

;;;; apheleia

(require 'apheleia)

(setq apheleia-formatters-respect-indent-level nil)

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

(global-company-mode 1)

(keymap-unset company-active-map "M-n" t)
(keymap-unset company-active-map "M-p" t)

(keymap-set company-mode-map "C-c c" #'company-complete)

(defvar init-company-minibuffer-backends '(company-capf))
(defvar init-company-minibuffer-frontends '(company-pseudo-tooltip-frontend company-preview-if-just-one-frontend))

(defun init-minibuffer-set-company ()
  "Set company in minibuffer."
  (setq-local company-backends init-company-minibuffer-backends)
  (setq-local company-frontends init-company-minibuffer-frontends)
  (when global-company-mode
    (company-mode 1)))

(add-hook 'minibuffer-mode-hook #'init-minibuffer-set-company)

(defun init-around-company-capf-set-styles (func &rest args)
  "Set completion styles for `company-capf'.
FUNC ARGS see `company-capf'."
  (let ((completion-styles '(basic partial-completion)))
    (apply func args)))

(advice-add 'company-capf :around #'init-around-company-capf-set-styles)

;;;; eglot

(require 'eglot)

(setq eglot-extend-to-xref t)

;;;; abbrev

(require 'abbrev)

(setq abbrev-file-name (f-expand "abbrevs.el" priv-directory))

(setq-default abbrev-mode t)

(init-diminish-minor-mode 'abbrev-mode)

(defvar init-abbrev-writing-file nil)

(defun init-around-write-abbrev-file (func &rest args)
  "Set `init-abbrev-writing-file' around `write-abbrev-file'.
FUNC and ARGS see `write-abbrev-file'."
  (let ((init-abbrev-writing-file t))
    (apply func args)))

(defun init-around-abbrev-get (func sym name)
  "Check `init-abbrev-writing-file' around `abbrev-get'.
Return 0 for abbrev count while writing abbrevs file.
FUNC, SYM and NAME see `abbrev-get'."
  (if (and init-abbrev-writing-file (eq name :count))
      0
    (funcall func sym name)))

(advice-add #'write-abbrev-file :around #'init-around-write-abbrev-file)
(advice-add #'abbrev-get :around #'init-around-abbrev-get)

;;;; tempel

(require 'tempel)

(setq tempel-path (f-expand "templates.eld" priv-directory))

(keymap-set tempel-map "M-n" #'tempel-next)
(keymap-set tempel-map "M-p" #'tempel-previous)

(global-tempel-abbrev-mode 1)



;;; tools

;;;; dired

(require 'dired)
(require 'dired-x)

(setq dired-dwim-target t)
(setq dired-auto-revert-buffer t)
(setq dired-listing-switches "-lha")

(put 'dired-jump 'repeat-map nil)

(keymap-set ctl-x-4-map "j" #'dired-jump-other-window)

(keymap-set project-prefix-map "j" #'project-dired)

(keymap-set dired-mode-map "O" #'dired-omit-mode)

(evil-define-key 'normal dired-mode-map
  "O" #'dired-omit-mode)

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

;;;; diff

(require 'ediff)

(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;;;; comint

(require 'comint)

(evil-define-key 'insert comint-mode-map
  (kbd "M-r") #'comint-history-isearch-backward-regexp)

;;;; eshell

(require 'eshell)
(require 'em-hist)
(require 'em-cmpl)
(require 'em-dirs)
(require 'em-alias)

(setq eshell-aliases-file (f-expand "eshell-alias.esh" priv-directory))

(defun init-eshell-set-outline ()
  "Set outline vars for Eshell."
  (setq-local outline-regexp "^[^#$\n]* [#$] ")
  (setq-local outline-level (lambda () 1)))

(add-hook 'eshell-mode-hook #'init-eshell-set-outline)

(keymap-unset eshell-cmpl-mode-map "C-M-i" t)

(declare-function evil-collection-eshell-escape-stay "evil-collection-eshell")
(advice-add #'evil-collection-eshell-escape-stay :override #'ignore)

(evil-define-key 'insert eshell-hist-mode-map
  (kbd "M-r") #'eshell-previous-matching-input)

(defun init-eshell-dwim-find-buffer ()
  "Find eshell dwim buffer."
  (->> (buffer-list)
       (--first
        (and (eq (buffer-local-value 'major-mode it) 'eshell-mode)
             (s-starts-with? eshell-buffer-name (buffer-name it))
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

;;;; editor

(require 'with-editor)

(add-hook 'shell-mode-hook #'with-editor-export-editor)
(add-hook 'eshell-mode-hook #'with-editor-export-editor)

;;;; git

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

(require 'magit)

(keymap-set vc-prefix-map "v" #'magit-status)
(keymap-set vc-prefix-map "V" #'magit-status-here)
(keymap-set vc-prefix-map "f" #'magit-find-file)
(keymap-set vc-prefix-map "b" #'magit-blame-addition)
(keymap-set vc-prefix-map "n" #'magit-blob-next)
(keymap-set vc-prefix-map "p" #'magit-blob-previous)
(keymap-set vc-prefix-map "d" #'magit-diff-buffer-file)
(keymap-set vc-prefix-map "D" #'magit-diff)
(keymap-set vc-prefix-map "l" #'magit-log-buffer-file)
(keymap-set vc-prefix-map "L" #'magit-log)
(keymap-set vc-prefix-map "?" #'magit-file-dispatch)

(keymap-set project-prefix-map "v" #'magit-project-status)

;;;; spell

(require 'ispell)

(setq ispell-dictionary "american")



;;; bindings

;;;; project

(setq project-switch-commands
      '((project-find-file "Find File")
        (project-find-dir "Find Dir")
        (project-switch-to-buffer "Switch To Buffer")
        (project-dired "Dired")
        (project-eshell "Eshell")
        (magit-project-status "Magit")))

;;;; minors

(defvar-keymap init-minor-map
  "a s" #'auto-save-visited-mode
  "a r" #'auto-revert-mode
  "f s" #'flyspell-mode
  "f m" #'flymake-mode
  "t" #'toggle-truncate-lines
  "v" #'visual-line-mode
  "h" #'hl-line-mode
  "w" #'whitespace-mode
  "l" #'display-line-numbers-mode
  "L" #'init-toggle-line-numbers-type
  "g" #'init-goggles-mode
  "c" #'init-capslock-mode
  "n" #'init-qwerty-prog-mode)

(keymap-global-set "C-c m" init-minor-map)

;;;; apps

(defvar-keymap init-app-map)

(keymap-global-set "C-c \\" init-app-map)

;;;; leaders

(defvar init-leader-key "SPC")
(defvar init-leader-state '(motion normal))

(defun init-leader-bindings (clauses)
  "Transforms `define-key' CLAUSES to binding alist."
  (->> clauses
       (-partition 2)
       (--map
        (cons (kbd (concat init-leader-key " " (car it))) (cadr it)))))

(defun init-leader-set (keymap &rest clauses)
  "Define leader binding CLAUSES in KEYMAP."
  (declare (indent defun))
  (dolist (binding (init-leader-bindings clauses))
    (evil-define-key* init-leader-state keymap (car binding) (cdr binding))))

(defun init-leader-global-set (&rest clauses)
  "Define leader binding CLAUSES in `init-evil-override-mode-map'."
  (apply #'init-leader-set (cons init-evil-override-mode-map clauses)))

(defun init-leader-minor-mode-set (mode &rest clauses)
  "Define leader binding CLAUSES assoc with minor MODE."
  (declare (indent defun))
  (dolist (binding (init-leader-bindings clauses))
    (evil-define-minor-mode-key init-leader-state mode (car binding) (cdr binding))))

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

(init-leader-global-set
 "SPC" #'consult-buffer
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
 "<left>" #'previous-buffer
 "<right>" #'next-buffer
 "b" #'switch-to-buffer
 "k" #'kill-buffer
 "f" #'find-file
 "d" #'dired
 "j" #'dired-jump
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
 "m" init-minor-map
 "\\" init-app-map)

(init-leader-global-set
 "$" #'ispell-word
 "%" #'query-replace-regexp
 "=" #'apheleia-format-buffer
 "+" #'delete-trailing-whitespace
 "." #'xref-find-definitions
 "?" #'xref-find-references
 "," #'xref-go-back
 "i" #'imenu
 "l" #'init-consult-outline
 "9" #'init-wrap-pair-common
 "(" #'init-wrap-pair
 "[" #'init-wrap-pair
 "{" #'init-wrap-pair
 "<" #'init-wrap-pair
 "'" #'init-wrap-pair
 "`" #'init-wrap-pair
 "\"" #'init-wrap-pair)



;;; lang

;;;; elisp

(defvar init-elisp-hooks
  '(emacs-lisp-mode-hook lisp-interaction-mode-hook))

(require 'ielm)

(defun init-ielm-other-window ()
  "Switch to elisp repl other window."
  (interactive)
  (pop-to-buffer (get-buffer-create "*ielm*"))
  (ielm))

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (keymap-set map "C-c C-k" #'eval-buffer)
  (keymap-set map "C-C C-l" #'load-file)
  (keymap-set map "C-c C-z" #'init-ielm-other-window)
  (define-key map [remap display-local-help] #'eldoc-doc-buffer))

;;;;; outline

(defun init-elisp-outline-level ()
  "Return level of current outline heading."
  (if (looking-at ";;\\([;*]+\\)")
      (- (match-end 1) (match-beginning 1))
    (funcall outline-level)))

(defun init-elisp-set-outline ()
  "Set outline vars."
  (setq-local outline-regexp ";;[;*]+[\s\t]+")
  (setq-local outline-level #'init-elisp-outline-level))

(dolist (hook init-elisp-hooks)
  (add-hook hook #'init-elisp-set-outline))

;;;;; dash

(dash-register-info-lookup)

(global-dash-fontify-mode 1)

;;;;; flymake

(setq trusted-content (list (f-slash (f-abbrev init-lisp-directory))))

(setq elisp-flymake-byte-compile-load-path load-path)

(require 'package-lint-flymake)

(add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup)

;;;;; macrostep

(require 'macrostep)

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map inferior-emacs-lisp-mode-map))
  (keymap-set map "C-c e" #'macrostep-expand))

;;;; org

(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'embark-org)

(setq org-special-ctrl-a/e t)

(defun init-org-modify-syntax ()
  "Modify `org-mode' syntax table."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(add-hook 'org-mode-hook #'init-org-modify-syntax)

(keymap-set org-mode-map "C-c C-'" #'org-edit-special)
(keymap-set org-src-mode-map "C-c C-'" #'org-edit-src-exit)
(keymap-set org-src-mode-map "C-c C-c" #'org-edit-src-exit)

(init-leader-set org-mode-map
  "n b" #'org-narrow-to-block
  "n s" #'org-narrow-to-subtree)

(keymap-set init-app-map "a" #'org-agenda)
(keymap-set init-app-map "c" #'org-capture)
(keymap-set init-app-map "w" #'org-store-link)
(keymap-set init-app-map "o" #'org-open-at-point-global)

(keymap-global-set "C-c l" #'org-insert-link-global)

(defun init-org-echo-link ()
  "Echo org link in minibuffer."
  (interactive)
  (when (org-in-regexp org-link-any-re)
    (let (message-log-max)
      (message (match-string-no-properties 0)))))

(keymap-set embark-org-link-map "e" #'init-org-echo-link)

;;;;; agenda

(setq org-directory (f-expand "org" user-emacs-directory))
(setq org-agenda-files (list org-directory))
(setq org-default-notes-file (f-expand "inbox.org" org-directory))

(setq org-capture-templates
      '(("u" "Task Inactive" entry (file+headline "" "Tasks") "* TODO %?\n%U\n%a")
        ("t" "Task Active" entry (file+headline "" "Tasks") "* TODO %?\n%T\n%a")))

(evil-set-initial-state 'org-agenda-mode 'motion)

(evil-define-key 'motion org-agenda-mode-map
  (kbd "RET") #'org-agenda-switch-to
  (kbd "<return>") #'org-agenda-switch-to
  (kbd "TAB") #'org-agenda-goto
  (kbd "<tab>") #'org-agenda-goto
  (kbd "M-j") #'org-agenda-drag-line-forward
  (kbd "M-k") #'org-agenda-drag-line-backward
  "q" #'org-agenda-quit
  "Q" #'org-agenda-Quit
  "ZZ" #'org-agenda-quit
  "ZQ" #'org-agenda-exit
  "j" #'org-agenda-next-line
  "k" #'org-agenda-previous-line
  "gj" #'org-agenda-next-item
  "gk" #'org-agenda-previous-item
  "u" #'org-agenda-undo
  "gr" #'org-agenda-redo
  "gR" #'org-agenda-redo-all
  "m" #'org-agenda-bulk-toggle
  "M" #'org-agenda-bulk-mark-all
  "U" #'org-agenda-bulk-unmark-all
  "T" #'org-agenda-bulk-toggle-all
  "%" #'org-agenda-bulk-mark-regexp
  "x" #'org-agenda-bulk-action
  "d" #'org-agenda-kill
  "t" #'org-agenda-todo
  "c" #'org-agenda-capture
  "I" #'org-agenda-clock-in
  "O" #'org-agenda-clock-out
  "C" #'org-agenda-clock-cancel
  "J" #'org-agenda-priority-down
  "K" #'org-agenda-priority-up
  "H" #'org-agenda-do-date-earlier
  "L" #'org-agenda-do-date-later
  "S" #'org-agenda-schedule
  "D" #'org-agenda-deadline
  "g." #'org-agenda-goto-today
  "gd" #'org-agenda-goto-date
  "gv" #'org-agenda-view-mode-dispatch)

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

;;;; web

(require 'sgml-mode)
(require 'css-mode)
(require 'js)

(setq css-indent-offset 2)
(setq js-indent-level 2)

;;;; python

(require 'python)

(defvar init-python-modes '(python-mode python-ts-mode inferior-python-mode))
(defvar init-python-mode-hooks '(python-base-mode-hook inferior-python-mode-hook))

(setq python-shell-interpreter "python")
(setq python-shell-interpreter-args "-m IPython --simple-prompt")

(dolist (mode init-python-modes)
  (add-to-list 'init-evil-eval-function-alist `(,mode . python-shell-send-region)))



;;; end

(provide 'init-emacs)
;;; init-emacs.el ends here
