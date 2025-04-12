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

(defun init-dwim-thing-at-point ()
  "Get thing at point dwim."
  (or (init-region-content) (thing-at-point 'symbol)))

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

(defun init-dwim-switch-to-buffer-split-window (arg buffer)
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

(defun init-dwim-switch-to-buffer (arg buffer)
  "Do switch to BUFFER smartly, with interactive ARG.
Without universal ARG, open in current window.
With one or more universal ARG, open in other window."
  (if arg
      (switch-to-buffer-other-window buffer)
    (switch-to-buffer buffer)))

(defun init-dwim-find-file (arg file)
  "Do find FILE smartly, with interactive ARG.
Without universal ARG, open in current window.
With one or more universal ARG, open in other window."
  (if arg
      (find-file-other-window file)
    (find-file file)))

(defun init-project-directory ()
  "Get current project directory."
  (-when-let (project (project-current))
    (project-root project)))

(defun init-project-or-default-directory ()
  "Get current project directory or default directory."
  (or (init-project-directory) default-directory))

(defun init-dwim-directory (arg prompt)
  "Get directory smartly.
With universal ARG read directory with PROMPT."
  (if arg
      (read-directory-name prompt)
    (init-project-or-default-directory)))

(defun init-project-file-relative-name (file)
  "Get relative FILE of project, or nil."
  (-when-let (directory (init-project-directory))
    (file-relative-name file directory)))

(defun init-dwim-project-find-file (arg file)
  "Find FILE in project.
ARG see `init-dwim-find-file'."
  (let ((default-directory (init-project-or-default-directory)))
    (init-dwim-find-file arg file)))



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

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "save/" user-emacs-directory) t))
      lock-file-name-transforms
      `((".*" ,(expand-file-name "lock/" user-emacs-directory) t))
      backup-directory-alist
      `((".*" . ,(expand-file-name "backup/" user-emacs-directory))))

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

(setq recentf-max-saved-items 200)

(recentf-mode 1)

(keymap-set ctl-x-r-map "e" #'recentf-open)

;;;; saveplace

(require 'saveplace)

(save-place-mode 1)

;;;; so-long

(require 'so-long)

(global-so-long-mode 1)

;;;; revert

;; r: revert buffer
;; v: update vc state
;; f: update font lock state

(keymap-set goto-map "r" #'revert-buffer-quick)
(keymap-set goto-map "R" #'revert-buffer)
(keymap-set goto-map "v" #'vc-refresh-state)
(keymap-set goto-map "f" #'font-lock-update)



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

(require 'winner)

(winner-mode 1)

(require 'windmove)

(windmove-default-keybindings)

(require 'tab-bar)

(setq tab-bar-tab-hints t)
(setq tab-bar-select-tab-modifiers '(meta))

(tab-bar-mode 1)

(keymap-global-set "C-S-T" #'tab-bar-new-tab)
(keymap-global-set "C-S-W" #'tab-bar-close-tab)

(keymap-global-set "C--" #'global-text-scale-adjust)
(keymap-global-set "C-=" #'global-text-scale-adjust)



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

;;;; visual

(require 'goggles)

(init-diminish-minor-mode 'goggles-mode)

(add-hook 'text-mode-hook #'goggles-mode)
(add-hook 'prog-mode-hook #'goggles-mode)

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

(require 'page-break-lines)

(init-diminish-minor-mode 'page-break-lines-mode)

(global-page-break-lines-mode 1)

(require 'rainbow-delimiters)
(require 'rainbow-identifiers)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-identifiers-mode)
(add-hook 'dired-mode-hook #'rainbow-identifiers-mode)

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
(defvar evil-want-minibuffer)
(defvar evil-want-C-u-scroll)
(defvar evil-want-C-w-delete)
(defvar evil-want-Y-yank-to-eol)
(setq evil-want-keybinding nil)
(setq evil-want-minibuffer t)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-w-delete t)
(setq evil-want-Y-yank-to-eol t)

;; system related options, must be set before evil loaded.

(defvar evil-undo-system)
(defvar evil-search-module)
(setq evil-undo-system 'undo-redo)
(setq evil-search-module 'evil-search)

(require 'evil)

(setq evil-want-fine-undo t)
(setq evil-search-wrap nil)
(setq evil-symbol-word-search t)
(setq evil-respect-visual-line-mode t)

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

(keymap-set evil-window-map "<left>" #'winner-undo)
(keymap-set evil-window-map "<right>" #'winner-redo)

;;;; evil collection

;; must be set before evil collection loaded.

(defvar evil-collection-setup-minibuffer)
(setq evil-collection-setup-minibuffer t)

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

;;;; evil snipe

(require 'evil-snipe)

(setq evil-snipe-repeat-keys nil)
(setq evil-snipe-smart-case t)
(setq evil-snipe-skip-leading-whitespace t)

(init-diminish-minor-mode 'evil-snipe-local-mode)

(evil-snipe-mode 1)
(evil-snipe-override-mode 1)

;;;; evil goggles

(require 'evil-goggles)

(add-to-list 'evil-goggles--commands
             '(init-evil-operator-comment
               :face evil-goggles-commentary-face
               :switch evil-goggles-enable-commentary
               :advice evil-goggles--generic-async-advice))

(add-to-list 'evil-goggles--commands
             '(init-evil-operator-eval
               :face evil-goggles-commentary-face
               :switch evil-goggles-enable-commentary
               :advice evil-goggles--generic-async-advice))

(init-diminish-minor-mode 'evil-goggles-mode)

(evil-goggles-mode 1)

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

;;;; completion style

(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-extended-command-predicate #'command-completion-default-include-p)

(require 'orderless)

(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides nil)

;;;; completion meta

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
(keymap-set vertico-map "RET" #'vertico-directory-enter)
(keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
(keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)

(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(evil-collection-define-key 'normal 'vertico-map
  "gg" #'vertico-first
  "G"  #'vertico-last)

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

;;;; corfu

(require 'corfu)
(require 'corfu-history)

(setq corfu-on-exact-match 'show)

(global-corfu-mode 1)
(corfu-history-mode 1)

(add-to-list 'savehist-additional-variables 'corfu-history)

(defun init-corfu-set-auto ()
  "Set `corfu-auto'."
  (setq-local corfu-auto t))

(add-hook 'prog-mode-hook #'init-corfu-set-auto)
(add-hook 'eval-expression-minibuffer-setup-hook #'init-corfu-set-auto)

(keymap-unset corfu-map "M-n" t)
(keymap-unset corfu-map "M-p" t)
(keymap-set corfu-map "TAB" #'corfu-expand)

;;;; cape

(require 'cape)

(add-hook 'completion-at-point-functions #'cape-file)

(defvar-keymap init-cape-prefix-map
  "f" #'cape-file
  "h" #'cape-history)

(keymap-global-set "C-c c" init-cape-prefix-map)

;;;; consult

(require 'consult)
(require 'embark-consult)

(setq consult-preview-key '(:debounce 0.3 any))

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

(defvar init-consult-outline-history nil)

(defun init-consult-outline-candidates ()
  "Collect outline headings."
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
                       (mapconcat #'identity (reverse stack) "/"))))
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
  (consult--forbid-minibuffer)
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

(consult-customize
 consult-goto-line
 consult-line
 consult-line-multi
 consult-imenu
 consult-imenu-multi
 init-consult-outline
 :preview-key 'any)

(define-key init-consult-override-mode-map [remap goto-line] #'consult-goto-line)
(define-key init-consult-override-mode-map [remap imenu] #'consult-imenu)

(keymap-set search-map "s" #'consult-line)
(keymap-set search-map "S" #'consult-line-multi)
(keymap-set search-map "i" #'consult-imenu)
(keymap-set search-map "I" #'consult-imenu-multi)
(keymap-set search-map "l" #'init-consult-outline)
(keymap-set search-map "g" #'consult-ripgrep)
(keymap-set search-map "f" #'consult-fd)

(advice-add #'consult-line :after #'init-set-search-after-consult-line)

(defvar-keymap init-embark-consult-sync-search-map
  "s" #'consult-line
  "S" #'consult-line-multi
  "i" #'consult-imenu
  "I" #'consult-imenu-multi
  "l" #'init-consult-outline)

(defvar-keymap init-embark-consult-async-search-map
  "g" #'consult-ripgrep
  "f" #'consult-fd)

(defvar init-embark-consult-search-map
  (keymap-canonicalize
   (make-composed-keymap
    init-embark-consult-sync-search-map
    init-embark-consult-async-search-map)))

(fset 'init-embark-consult-sync-search-map init-embark-consult-sync-search-map)
(keymap-set embark-become-match-map "C" 'init-embark-consult-sync-search-map)
(fset 'init-embark-consult-search-map init-embark-consult-search-map)
(keymap-set embark-general-map "C" 'init-embark-consult-search-map)
(cl-pushnew 'init-embark-consult-async-search-map embark-become-keymaps)

(defun init-set-search-after-consult-line (&rest _args)
  "Set `evil-ex-search-pattern' after `consult-line'."
  (let ((pattern (car consult--line-history)))
    (setq evil-ex-search-pattern (list pattern t t))
    (evil-ex-nohighlight)))

(defun init-consult-line-dwim (&optional start)
  "Consult line of symbol at point.
START see `consult-line'."
  (interactive (list (not (not current-prefix-arg))))
  (-when-let (thing (init-dwim-thing-at-point))
    (setq this-command 'consult-line)
    (consult-line thing start)))

(defun init-consult-line-multi-dwim (&optional query)
  "Consult line of symbol at point.
QUERY see `consult-line-multi'."
  (interactive "P")
  (-when-let (thing (init-dwim-thing-at-point))
    (setq this-command 'consult-line-multi)
    (consult-line-multi query thing)))

(defun init-consult-ripgrep-dwim (&optional dir)
  "Consult line of symbol at point.
DIR see `consult-ripgrep'."
  (interactive "P")
  (-when-let (thing (init-dwim-thing-at-point))
    (setq this-command 'consult-ripgrep)
    (consult-ripgrep dir thing)))

(keymap-global-set "C-s"   #'init-consult-line-dwim)
(keymap-global-set "C-M-s" #'init-consult-line-multi-dwim)
(keymap-global-set "C-M-g" #'init-consult-ripgrep-dwim)

;;;;; corfu

(defun init-consult-corfu ()
  "Move corfu completions to minibuffer."
  (interactive)
  (pcase completion-in-region--data
    (`(,beg ,end ,table ,pred ,extras)
     (let ((completion-extra-properties extras)
           completion-cycle-threshold completion-cycling)
       (consult-completion-in-region beg end table pred)))))

(add-to-list 'corfu-continue-commands #'init-consult-corfu)

(keymap-set corfu-map "C-s" #'init-consult-corfu)



;;; help

(keymap-set help-map "L" #'view-lossage)
(keymap-set help-map "B" #'describe-keymap)
(keymap-set help-map "p" #'describe-package)
(keymap-set help-map "P" #'finder-by-keyword)

;;;; load

(keymap-unset help-map "t" t)
(keymap-set help-map "t f" #'load-file)
(keymap-set help-map "t l" #'load-library)
(keymap-set help-map "t t" #'load-theme)

(consult-customize consult-theme :preview-key '(:debounce 0.5 any))
(define-key init-consult-override-mode-map [remap load-theme] #'consult-theme)

;;;; find func

(require 'find-func)

;; TODO replace with `find-function-mode' after 30.1
;; (find-function-mode 1)
(find-function-setup-keys)

(keymap-set help-map "l" #'find-library)
(keymap-set help-map "4 l" #'find-library-other-window)
(keymap-set help-map "5 l" #'find-library-other-frame)
(keymap-set help-map "F" #'find-function)
(keymap-set help-map "4 F" #'find-function-other-window)
(keymap-set help-map "5 F" #'find-function-other-frame)
(keymap-set help-map "V" #'find-variable)
(keymap-set help-map "4 V" #'find-variable-other-window)
(keymap-set help-map "5 V" #'find-variable-other-frame)

;;;; elisp lookup

(defun init-describe-symbol-dwim ()
  "Describe symbol at point."
  (interactive)
  (-when-let (thing (init-dwim-thing-at-point))
    (describe-symbol (intern thing))))

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

(keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
(keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error)

;;;; eldoc

(require 'eldoc)

(setq eldoc-minor-mode-string nil)

;;;; xref

(require 'xref)

(setq xref-search-program 'ripgrep)

;;;; abbrev

(require 'abbrev)

(setq abbrev-file-name (expand-file-name "abbrevs.el" priv-directory))

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

(setq tempel-path (expand-file-name "templates.eld" priv-directory))

(keymap-set init-cape-prefix-map "s" #'tempel-complete)

(keymap-set tempel-map "M-n" #'tempel-next)
(keymap-set tempel-map "M-p" #'tempel-previous)

(global-tempel-abbrev-mode 1)

;;;; apheleia

(require 'apheleia)



;;; tools

;;;; dired

(require 'dired)
(require 'dired-x)

(setq dired-dwim-target t)
(setq dired-auto-revert-buffer t)
(setq dired-kill-when-opening-new-dired-buffer t)
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

(defun init-rg-dwim (arg)
  "RG dwim.
Without universal ARG, rg in project directory.
With one universal ARG, prompt for rg directory.
With two universal ARG, edit rg command."
  (interactive "P")
  (let* ((default-directory (init-dwim-directory arg "Search directory: "))
         (pattern-default (init-dwim-thing-at-point))
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

(setq eshell-aliases-file (expand-file-name "eshell-alias.esh" priv-directory))

(defun init-eshell-set-outline ()
  "Set outline vars for Eshell."
  (setq-local outline-regexp "^[^#$\n]* [#$] ")
  (setq-local outline-level (lambda () 1)))

(add-hook 'eshell-mode-hook #'init-eshell-set-outline)

(keymap-unset eshell-cmpl-mode-map "C-M-i" t)

(add-hook 'eshell-mode-hook #'init-corfu-set-auto)

(declare-function evil-collection-eshell-escape-stay "evil-collection-eshell")
(advice-add #'evil-collection-eshell-escape-stay :override #'ignore)

(evil-define-key 'insert eshell-hist-mode-map
  (kbd "M-r") #'eshell-previous-matching-input)

(defun init-eshell-dwim-find-buffer ()
  "Find eshell dwim buffer."
  (->> (buffer-list)
       (--first
        (and (eq (buffer-local-value 'major-mode it) 'eshell-mode)
             (string-prefix-p eshell-buffer-name (buffer-name it))
             (not (get-buffer-process it))
             (not (get-buffer-window it))))))

(defun init-eshell-dwim-get-buffer ()
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
ARG see `init-dwim-switch-to-buffer-split-window'."
  (interactive "P")
  (init-dwim-switch-to-buffer-split-window arg (init-eshell-dwim-get-buffer)))

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
  "r d" #'rainbow-delimiters-mode
  "r i" #'rainbow-identifiers-mode
  "t" #'toggle-truncate-lines
  "v" #'visual-line-mode
  "h" #'hl-line-mode
  "w" #'whitespace-mode
  "l" #'display-line-numbers-mode
  "L" #'init-toggle-line-numbers-type
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

(defun init-magic-C-x ()
  "Magic control X."
  (interactive)
  (init-magic "C-x"))

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
 "x" #'init-magic-C-x
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
 "f" #'find-file
 "d" #'dired
 "j" #'dired-jump
 "k" #'kill-buffer
 "e" #'init-eshell-dwim
 "w" evil-window-map
 "4" ctl-x-4-map
 "5" ctl-x-5-map
 "t" tab-prefix-map
 "p" project-prefix-map
 "v" vc-prefix-map
 "r" ctl-x-r-map
 "h" help-map
 "g" goto-map
 "s" search-map
 "a" abbrev-map
 "n" narrow-map
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
 "i" #'consult-imenu
 "I" #'consult-imenu-multi
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
  (keymap-set map "C-c C-z" #'init-ielm-other-window))

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

(setq trusted-content (list (file-name-as-directory (abbreviate-file-name init-lisp-directory))))

(setq elisp-flymake-byte-compile-load-path load-path)

(require 'package-lint-flymake)

(add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup)

;;;;; macrostep

(require 'macrostep)

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map inferior-emacs-lisp-mode-map))
  (keymap-set map "C-c e" #'macrostep-expand))

;;;; org

(require 'org)
(require 'org-capture)
(require 'embark-org)

(setq org-directory (expand-file-name "org" user-emacs-directory))
(setq org-agenda-files (list org-directory))
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

(setq org-capture-templates
      '(("t" "Task" entry (file+headline "" "Tasks") "* TODO %?\n%U\n%a")))

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
(keymap-global-set "C-c C-l" #'org-insert-link-global)

(defun init-org-echo-link ()
  "Echo org link in minibuffer."
  (interactive)
  (when (org-in-regexp org-link-any-re)
    (let (message-log-max)
      (message (match-string-no-properties 0)))))

(keymap-set embark-org-link-map "e" #'init-org-echo-link)

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



;;; end

(provide 'init-emacs)
;;; init-emacs.el ends here
