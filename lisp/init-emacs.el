;;; init-emacs.el --- Init Emacs -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Emacs itself.

(require 'cl-lib)
(require 'dash)
(require 'init-core)

;;; Code:

;;; essentials

(prefer-coding-system 'utf-8)

(setq! system-time-locale "C")

(setq! read-process-output-max (* 1024 1024))

;;; utils

(defun init-diminish-minor-mode (mode)
  "Diminish MODE lighter."
  (setq minor-mode-alist
        (->> minor-mode-alist
             (--remove (eq (car it) mode)))))

(defun init-symbol-at-point ()
  "Get symbol at point."
  (interactive)
  (or (symbol-at-point) (user-error "No symbol at point")))

(defun init-dwim-switch-to-buffer (buffer arg)
  "Do goto BUFFER smartly, with interactive ARG.
Without universal ARG, open in split window.
With one universal ARG, open other window.
With two or more universal ARG, open in current window."
  (cond ((> (prefix-numeric-value arg) 4)
         (switch-to-buffer buffer))
        (arg
         (switch-to-buffer-other-window buffer))
        (t
         (let ((parent (window-parent (selected-window))))
           (cond ((window-left-child parent)
                  (select-window (split-window-vertically))
                  (switch-to-buffer buffer))
                 ((window-top-child parent)
                  (select-window (split-window-horizontally))
                  (switch-to-buffer buffer))
                 (t
                  (switch-to-buffer-other-window buffer)))))))

;;; files

(setq! project-mode-line t)
(setq! vc-handled-backends '(Git))
(setq! vc-make-backup-files t)
(setq! version-control t)
(setq! backup-by-copying t)
(setq! delete-old-versions t)
(setq! delete-by-moving-to-trash t)

(setq! auto-save-file-name-transforms
       `((".*" ,(expand-file-name "save/" user-emacs-directory) t)))
(setq! lock-file-name-transforms
       `((".*" ,(expand-file-name "lock/" user-emacs-directory) t)))
(setq! backup-directory-alist
       `((".*" . ,(expand-file-name "backup/" user-emacs-directory))))

(setq! auto-save-visited-interval 1)

(add-to-list 'minor-mode-alist '(auto-save-visited-mode " ASave"))

(auto-save-visited-mode 1)

(setq! recentf-max-saved-items 200)

(require 'recentf)

(recentf-mode 1)

(keymap-set ctl-x-r-map "e" #'recentf-open)

;;; ui

;;;; graphic elements

(setq! inhibit-startup-screen t)
(setq! initial-scratch-message nil)

(setq! use-dialog-box nil)
(setq! use-file-dialog nil)

(setq! ring-bell-function #'ignore)

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

(setq! tab-bar-tab-hints t)
(setq! tab-bar-select-tab-modifiers '(meta))

(require 'tab-bar)

(tab-bar-mode 1)

(keymap-global-set "C-S-T" #'tab-bar-new-tab)
(keymap-global-set "C-S-W" #'tab-bar-close-tab)

(keymap-global-set "C--" #'text-scale-decrease)
(keymap-global-set "C-=" #'text-scale-increase)

;;; edit

;;;; commands

(setq! disabled-command-function nil)

(require 'repeat)

(repeat-mode 1)

(require 'embark)

(keymap-global-set "M-o" #'embark-act)

(keymap-global-set "C-SPC" #'toggle-input-method)

;;;; indent

(setq-default indent-tabs-mode nil)

(setq-default truncate-lines t)

(setq! word-wrap-by-category t)

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

;;; evil

(setq! evil-want-keybinding nil)
(setq! evil-want-minibuffer t)
(setq! evil-want-C-u-scroll t)
(setq! evil-want-Y-yank-to-eol t)
(setq! evil-want-fine-undo t)
(setq! evil-undo-system 'undo-redo)
(setq! evil-search-module 'evil-search)
(setq! evil-symbol-word-search t)
(setq! evil-respect-visual-line-mode t)

(require 'evil)

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

(keymap-set evil-window-map "<left>" #'winner-undo)
(keymap-set evil-window-map "<right>" #'winner-redo)

(setq! evil-collection-setup-minibuffer t)

(require 'evil-collection)

(evil-collection-init)

(init-diminish-minor-mode 'evil-collection-unimpaired-mode)

(require 'evil-surround)

(add-to-list 'evil-surround-pairs-alist '(?r . ("[" . "]")))
(add-to-list 'evil-surround-pairs-alist '(?a . ("<" . ">")))
(add-to-list 'evil-surround-pairs-alist '(?$ . ("$" . "$")))
(add-to-list 'evil-surround-pairs-alist '(?# . ("#{" . "}")))

(keymap-set evil-inner-text-objects-map "r" #'evil-inner-bracket)
(keymap-set evil-outer-text-objects-map "r" #'evil-a-bracket)
(keymap-set evil-inner-text-objects-map "a" #'evil-inner-angle)
(keymap-set evil-outer-text-objects-map "a" #'evil-an-angle)

(global-evil-surround-mode 1)

(setq! evil-snipe-repeat-keys nil)
(setq! evil-snipe-smart-case t)
(setq! evil-snipe-skip-leading-whitespace t)

(require 'evil-snipe)

(init-diminish-minor-mode 'evil-snipe-local-mode)

(evil-snipe-mode 1)
(evil-snipe-override-mode 1)

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

(defun init-evil-escape ()
  ":imap jk <esc>."
  (interactive)
  (if (or executing-kbd-macro
          defining-kbd-macro
          (sit-for 0.15 t))
      (insert ?j)
    (let ((event (read-event)))
      (if (= event ?k)
          (progn
            (setq this-command 'ignore
                  real-this-command 'ignore)
            (push 'escape unread-command-events))
        (insert ?j)
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
  (when-let ((eval-function (cdr (assq major-mode init-evil-eval-function-alist))))
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

(require 'delsel)

(define-key minibuffer-local-map [remap quit-window] #'minibuffer-keyboard-quit)

(evil-define-key 'insert minibuffer-mode-map
  (kbd "M-r") #'previous-matching-history-element)

(setq! enable-recursive-minibuffers t)
(setq! completion-ignore-case t)
(setq! read-buffer-completion-ignore-case t)
(setq! read-file-name-completion-ignore-case t)

(require 'orderless)

(defun init-minibuffer-set-orderless ()
  "Setup orderless."
  (setq-local completion-category-defaults nil)
  (setq-local completion-styles '(orderless)))

(add-hook 'minibuffer-setup-hook #'init-minibuffer-set-orderless)

(require 'marginalia)

(marginalia-mode 1)

;;;; vertico

(require 'vertico)
(require 'vertico-multiform)
(require 'vertico-directory)
(require 'vertico-repeat)
(require 'vertico-suspend)

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

(defun init-vertico-embark-preview ()
  "Previews candidate in vertico buffer."
  (interactive)
  (save-selected-window
    (let ((embark-quit-after-action nil))
      (embark-dwim))))

(keymap-set vertico-map "C-j" #'init-vertico-embark-preview)
(keymap-set vertico-map "C-x C-s" #'embark-export)

;;;; consult

(setq! consult-preview-key '(:debounce 0.3 any))

(require 'consult)
(require 'consult-imenu)

(consult-customize
 consult-line
 consult-line-multi
 consult-imenu
 consult-imenu-multi
 consult-outline
 :preview-key 'any)

(setq! completion-in-region-function #'consult-completion-in-region)

(defvar-keymap init-consult-override-mode-map)

(define-minor-mode init-consult-override-mode
  "Override consult commands."
  :group 'init
  :global t
  :keymap init-consult-override-mode-map)

(init-consult-override-mode 1)

(define-key init-consult-override-mode-map [remap previous-matching-history-element] #'consult-history)
(define-key init-consult-override-mode-map [remap eshell-previous-matching-input] #'consult-history)
(define-key init-consult-override-mode-map [remap comint-history-isearch-backward-regexp] #'consult-history)
(define-key init-consult-override-mode-map [remap yank-pop] #'consult-yank-pop)
(define-key init-consult-override-mode-map [remap goto-line] #'consult-goto-line)

(defun init-consult-symbol-at-point (&optional start)
  "Consult line of symbol at point.
START see `consult-line'."
  (interactive (list (not (not current-prefix-arg))))
  (-> (init-symbol-at-point) symbol-name (consult-line start)))

(consult-customize init-consult-symbol-at-point :preview-key 'any)

(keymap-global-set "C-s" #'init-consult-symbol-at-point)

(keymap-set search-map "s" #'consult-line)
(keymap-set search-map "S" #'consult-line-multi)
(keymap-set search-map "i" #'consult-imenu)
(keymap-set search-map "I" #'consult-imenu-multi)
(keymap-set search-map "l" #'consult-outline)
(keymap-set search-map "g" #'consult-ripgrep)
(keymap-set search-map "f" #'consult-fd)

(keymap-set search-map "m" 'evil-collection-consult-mark)
(keymap-set search-map "j" 'evil-collection-consult-jump-list)

;;;; isearch

(setq! isearch-lazy-count t)
(setq! isearch-allow-scroll t)
(setq! isearch-allow-motion t)
(setq! isearch-yank-on-move t)
(setq! isearch-motion-changes-direction t)
(setq! isearch-repeat-on-direction-change t)

;;;; goto

(keymap-set goto-map "r" #'revert-buffer-quick)
(keymap-set goto-map "R" #'revert-buffer)
(keymap-set goto-map "v" #'vc-refresh-state)
(keymap-set goto-map "f" #'font-lock-update)

(keymap-set goto-map "<left>" #'previous-buffer)
(keymap-set goto-map "<right>" #'next-buffer)

;;; help

(require 'find-func)

(find-function-setup-keys)

(keymap-set help-map "B" #'describe-keymap)

(keymap-set help-map "p" #'describe-package)
(keymap-set help-map "P" #'finder-by-keyword)

(keymap-set help-map "l" #'find-library)
(keymap-set help-map "4 l" #'find-library-other-window)
(keymap-set help-map "5 l" #'find-library-other-frame)

(keymap-set help-map "L" #'view-lossage)

(keymap-set help-map "t" #'load-theme)

(consult-customize consult-theme :preview-key '(:debounce 0.5 any))

(define-key init-consult-override-mode-map [remap load-theme] #'consult-theme)

(defun init-describe-symbol-at-point ()
  "Describe symbol at point."
  (interactive)
  (describe-symbol (init-symbol-at-point)))

(setq! evil-lookup-func #'init-describe-symbol-at-point)

;;; prog

;;;; project

(setq! project-switch-use-entire-map t)

(require 'project)

(keymap-set project-prefix-map "j" #'project-dired)

(add-to-list 'project-switch-commands '(project-dired "Dired jump"))

(defun init-replace-project-switch-command (orig after)
  "Replace ORIG with AFTER in `project-switch-commands'."
  (setq project-switch-commands
        (->> project-switch-commands
             (--map
              (if (eq (car it) orig) (cons after (cdr it)) it)))))

;;;; vc

(require 'magit)

(keymap-set vc-prefix-map "v" #'magit-status)
(keymap-set vc-prefix-map "V" #'magit-status-here)
(keymap-set vc-prefix-map "b" #'magit-blame-addition)
(keymap-set vc-prefix-map "n" #'magit-blob-next)
(keymap-set vc-prefix-map "p" #'magit-blob-previous)
(keymap-set vc-prefix-map "d" #'magit-diff-buffer-file)
(keymap-set vc-prefix-map "D" #'magit-diff)
(keymap-set vc-prefix-map "l" #'magit-log-buffer-file)
(keymap-set vc-prefix-map "L" #'magit-log)
(keymap-set vc-prefix-map "?" #'magit-file-dispatch)

(keymap-set project-prefix-map "v" #'magit-project-status)

(init-replace-project-switch-command 'project-vc-dir 'magit-project-status)

;;;; xref

(setq! xref-search-program 'ripgrep)

(require 'xref)

;;;; eldoc

(setq! eldoc-minor-mode-string nil)

(require 'eldoc)

;;;; abbrev

(setq! only-global-abbrevs t)

(require 'abbrev)

(init-diminish-minor-mode 'abbrev-mode)

(setq-default abbrev-mode t)

;;;; yasnippet

(setq! yas-alias-to-yas/prefix-p nil)

(require 'yasnippet)

(init-diminish-minor-mode 'yas-minor-mode)

(yas-global-mode 1)

(keymap-set yas-minor-mode-map "C-c s" #'yas-expand-from-trigger-key)

(evil-define-key 'insert yas-minor-mode-map
  (kbd "M-s") #'yas-insert-snippet)

(keymap-set abbrev-map "n" #'yas-new-snippet)
(keymap-set abbrev-map "s" #'yas-insert-snippet)
(keymap-set abbrev-map "v" #'yas-visit-snippet-file)

(add-hook 'snippet-mode-hook #'whitespace-mode)

(require 'consult-yasnippet)

(define-key init-consult-override-mode-map [remap yas-insert-snippet] #'consult-yasnippet)

;;;; company

(setq! company-lighter-base "Company")
(setq! company-selection-wrap-around t)
(setq! company-show-quick-access t)
(setq! company-dabbrev-downcase nil)
(setq! company-dabbrev-ignore-case t)
(setq! company-dabbrev-code-ignore-case t)

(setq! company-frontends
       '(company-pseudo-tooltip-frontend
         company-preview-common-frontend
         company-echo-metadata-frontend))

(setq! company-backends
       '(company-files
         (company-capf :with company-yasnippet)
         (company-dabbrev-code company-keywords :with company-yasnippet)
         (company-dabbrev company-yasnippet)))

(require 'company)

(global-company-mode 1)

(keymap-set company-mode-map "C-c c" #'company-complete)

(require 'consult-company)

(define-key init-consult-override-mode-map [remap company-search-candidates] #'consult-company)

;;;; flymake

(require 'flymake)
(require 'flymake-proc)

(remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)

(keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
(keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error)

(require 'consult-flymake)

(keymap-set search-map "v" #'consult-flymake)

;;;; apheleia

(require 'apheleia)

;;; tools

;;;; dired

(setq! dired-dwim-target t)
(setq! dired-auto-revert-buffer t)
(setq! dired-kill-when-opening-new-dired-buffer t)
(setq! dired-listing-switches "-lha")

(require 'dired)
(require 'dired-x)

(put 'dired-jump 'repeat-map nil)

(keymap-set ctl-x-4-map "j" #'dired-jump-other-window)

(keymap-set dired-mode-map "O" #'dired-omit-mode)

(evil-define-key 'normal dired-mode-map
  "O" #'dired-omit-mode)

;;;; grep

(setq! wgrep-auto-save-buffer t)
(setq! wgrep-change-readonly-file t)

(require 'wgrep)

;;;; diff

(require 'ediff)

(setq! ediff-window-setup-function #'ediff-setup-windows-plain)

;;;; comint

(require 'comint)

(evil-define-key 'insert comint-mode-map
  (kbd "M-r") #'comint-history-isearch-backward-regexp)

;;;; eshell

(require 'eshell)
(require 'em-hist)
(require 'em-dirs)

(defun init-eshell-set-outline ()
  "Set outline vars."
  (setq-local outline-regexp "^[^#$\n]* [#$] ")
  (setq-local outline-level (lambda () 1)))

(defun init-eshell-set-company ()
  "Clean company backends."
  (setq-local company-backends '(company-files (company-dabbrev company-yasnippet))))

(add-hook 'eshell-mode-hook #'with-editor-export-editor)
(add-hook 'eshell-mode-hook #'init-eshell-set-outline)
(add-hook 'eshell-mode-hook #'init-eshell-set-company)

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
ARG see `init-dwim-switch-to-buffer'."
  (interactive "P")
  (-> (init-eshell-dwim-get-buffer)
      (init-dwim-switch-to-buffer arg)))

(defun init-eshell-dwim-project (&optional arg)
  "Do open eshell smartly in project.
ARG see `init-dwim-switch-to-buffer'."
  (interactive "P")
  (let* ((project (project-current))
         (default-directory (if project (project-root project) default-directory))
         (eshell-buffer-name (project-prefixed-buffer-name "eshell")))
    (init-eshell-dwim arg)))

(keymap-set project-prefix-map "e" #'init-eshell-dwim-project)

(init-replace-project-switch-command 'project-eshell 'init-eshell-dwim-project)

;;;; spell

(setq! ispell-dictionary "american")

;;; minors

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
  "L" #'init-toggle-line-numbers-type)

(keymap-global-set "C-x m" init-minor-map)

;;; leaders

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

(defun init-leader-universal-argument ()
  "Magic universal arguments for leader map."
  (interactive)
  (setq prefix-arg
        (list (if current-prefix-arg
                  (* 4 (prefix-numeric-value current-prefix-arg))
                4)))
  (set-transient-map (key-binding " ")))

(require 'god-mode)

(init-leader-global-set
 "SPC" #'consult-buffer
 "u" #'init-leader-universal-argument
 "x" #'god-mode-self-insert
 "c" #'god-mode-self-insert
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
 "m" init-minor-map
 "r" ctl-x-r-map
 "h" help-map
 "g" goto-map
 "s" search-map
 "a" abbrev-map
 "n" narrow-map)

(init-leader-global-set
 "%" #'query-replace-regexp
 "=" #'apheleia-format-buffer
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

;;; lang

;;;; elisp

(defvar init-elisp-hooks
  '(emacs-lisp-mode-hook lisp-interaction-mode-hook))

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

(keymap-set emacs-lisp-mode-map "C-c C-k" #'eval-buffer)
(keymap-set lisp-interaction-mode-map "C-c C-k" #'eval-buffer)

(dash-register-info-lookup)

(global-dash-fontify-mode 1)

(setq! elisp-flymake-byte-compile-load-path load-path)

(require 'package-lint-flymake)

(add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup)

(require 'macrostep)

(keymap-set emacs-lisp-mode-map "C-c e" #'macrostep-expand)
(keymap-set lisp-interaction-mode-map "C-c e" #'macrostep-expand)

;;;; markdown

(setq! markdown-special-ctrl-a/e t)
(setq! markdown-fontify-code-blocks-natively t)

(require 'markdown-mode)
(require 'edit-indirect)

(keymap-set markdown-mode-map "C-c C-'" #'markdown-edit-code-block)
(keymap-set edit-indirect-mode-map "C-c C-'" #'edit-indirect-commit)

(init-leader-set markdown-mode-map
  "n b" #'markdown-narrow-to-block
  "n s" #'markdown-narrow-to-subtree)

;;;; org

(setq! org-directory (expand-file-name "org" user-emacs-directory))
(setq! org-agenda-files (list org-directory))
(setq! org-default-notes-file (expand-file-name "inbox.org" org-directory))

(setq! org-capture-templates
       '(("t" "Task" entry (file+headline "" "Tasks") "* TODO %?\n%U\n%a")))

(setq! org-special-ctrl-a/e t)

(require 'org)

(defun init-org-modify-syntax ()
  "Modify `org-mode' syntax table."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(add-hook 'org-mode-hook #'init-org-modify-syntax)

(keymap-set org-mode-map "C-c l" #'org-toggle-link-display)

(keymap-set org-mode-map "C-c C-'" #'org-edit-special)
(keymap-set org-src-mode-map "C-c C-'" #'org-edit-src-exit)
(keymap-set org-src-mode-map "C-c C-c" #'org-edit-src-exit)

(keymap-set ctl-x-r-map "a" #'org-agenda)
(keymap-set ctl-x-r-map "c" #'org-capture)
(keymap-set ctl-x-r-map "l" #'org-store-link)

;;; end

(provide 'init-emacs)
;;; init-emacs.el ends here
