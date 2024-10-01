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

;;; files

(setq! vc-handled-backends '(Git))
(setq! vc-make-backup-files t)
(setq! version-control t)
(setq! backup-by-copying t)
(setq! delete-old-versions t)

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

(setq-default indent-tabs-mode nil)

(setq! word-wrap-by-category t)

(setq! disabled-command-function nil)

(require 'repeat)

(repeat-mode 1)

(keymap-global-set "C-SPC" #'toggle-input-method)

(require 'embark)

(keymap-global-set "M-o" #'embark-act)

;;; visual

(setq-default truncate-lines t)

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

(require 'goggles)

(init-diminish-minor-mode 'goggles-mode)

(add-hook 'text-mode-hook #'goggles-mode)
(add-hook 'prog-mode-hook #'goggles-mode)

;;; parens

(setq! sp-ignore-modes-list nil)

(require 'smartparens)
(require 'smartparens-config)

(sp-local-pair 'minibuffer-mode "'" nil :actions nil)

(init-diminish-minor-mode 'smartparens-mode)

(smartparens-global-mode 1)
(show-smartparens-global-mode 1)

(keymap-set smartparens-mode-map "C-M-f" #'sp-forward-sexp)
(keymap-set smartparens-mode-map "C-M-b" #'sp-backward-sexp)
(keymap-set smartparens-mode-map "C-M-d" #'sp-down-sexp)
(keymap-set smartparens-mode-map "C-M-u" #'sp-backward-up-sexp)
(keymap-set smartparens-mode-map "C-M-n" #'sp-next-sexp)
(keymap-set smartparens-mode-map "C-M-p" #'sp-previous-sexp)
(keymap-set smartparens-mode-map "C-M-k" #'sp-kill-sexp)
(keymap-set smartparens-mode-map "C-M-w" #'sp-copy-sexp)
(keymap-set smartparens-mode-map "C-M-SPC" #'sp-mark-sexp)
(keymap-set smartparens-mode-map "C-k" #'sp-kill-hybrid-sexp)
(keymap-set smartparens-mode-map "M-r" #'sp-splice-sexp-killing-around)
(keymap-set smartparens-mode-map "M-R" #'sp-splice-sexp-killing-backward)
(keymap-set smartparens-mode-map "M-s" #'sp-splice-sexp)
(keymap-set smartparens-mode-map "M-S" #'sp-split-sexp)
(keymap-set smartparens-mode-map "M-J" #'sp-join-sexp)
(keymap-set smartparens-mode-map "C-<right>" #'sp-forward-slurp-sexp)
(keymap-set smartparens-mode-map "C-<left>" #'sp-forward-barf-sexp)
(keymap-set smartparens-mode-map "C-M-<right>" #'sp-backward-barf-sexp)
(keymap-set smartparens-mode-map "C-M-<left>" #'sp-backward-slurp-sexp)

(defun init-sp-wrap-pair ()
  "Wrap following sexp with pairs."
  (interactive)
  (sp-wrap-with-pair (char-to-string last-command-event)))

;;; evil

(setq! evil-want-keybinding nil)
(setq! evil-want-minibuffer t)
(setq! evil-want-C-u-scroll t)
(setq! evil-want-Y-yank-to-eol t)
(setq! evil-want-fine-undo t)
(setq! evil-undo-system 'undo-redo)
(setq! evil-symbol-word-search t)
(setq! evil-respect-visual-line-mode t)

(require 'evil)

(evil-mode 1)

(defvar init-evil-adjust-cursor-disabled-commands
  '(sp-forward-sexp sp-previous-sexp forward-sexp forward-list))

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

(keymap-set evil-operator-state-map "o" #'evil-inner-symbol)
(keymap-set evil-operator-state-map "p" #'evil-inner-paragraph)

(keymap-set evil-window-map "<left>" #'winner-undo)
(keymap-set evil-window-map "<right>" #'winner-redo)

(setq! evil-collection-setup-minibuffer t)

(require 'evil-collection)

(evil-collection-init)

(init-diminish-minor-mode 'evil-collection-unimpaired-mode)

(require 'evil-surround)

(add-to-list 'evil-surround-pairs-alist '(?r . ("[" . "]")))
(add-to-list 'evil-surround-pairs-alist '(?a . ("<" . ">")))

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

(evil-goggles-use-diff-faces)

(init-diminish-minor-mode 'evil-goggles-mode)

(evil-goggles-mode 1)

(defun init-evil-escape ()
  ":imap jk <esc>."
  (interactive)
  (if (or executing-kbd-macro
          defining-kbd-macro
          (sit-for 0.15 'no-redisplay))
      (insert ?j)
    (let ((event (read-event)))
      (if (= event ?k)
          (progn
            (setq this-command 'ignore
                  real-this-command 'ignore)
            (push 'escape unread-command-events))
        (insert ?j)
        (push event unread-command-events)))))

(evil-define-motion init-evil-jump-item ()
  :jump t
  :type inclusive
  (let* ((thing (sp-get-thing))
         (beg (sp-get thing :beg))
         (end (1- (sp-get thing :end))))
    (goto-char (if (= (point) end) beg end))))

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
(keymap-set evil-motion-state-map "%" #'init-evil-jump-item)
(keymap-set evil-visual-state-map "m" #'init-evil-jump-item)
(keymap-set evil-operator-state-map "m" #'init-evil-jump-item)
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

(setq! enable-recursive-minibuffers t)
(setq! completion-ignore-case t)
(setq! read-buffer-completion-ignore-case t)
(setq! read-file-name-completion-ignore-case t)

(require 'amx)

(amx-mode 1)

(setq! ivy-count-format "(%d/%d) ")
(setq! ivy-use-selectable-prompt t)
(setq! ivy-use-virtual-buffers t)

(require 'ivy)
(require 'ivy-avy)
(require 'ivy-hydra)
(require 'swiper)
(require 'counsel)

(add-to-list 'ivy-completing-read-handlers-alist
             '(kill-buffer . completing-read-default))

(init-diminish-minor-mode 'ivy-mode)
(init-diminish-minor-mode 'counsel-mode)

(ivy-mode 1)
(counsel-mode 1)

(keymap-global-set "C-c b" #'ivy-resume)

(require 'delsel)

(define-key minibuffer-local-map [remap quit-window] #'minibuffer-keyboard-quit)
(define-key ivy-minibuffer-map [remap quit-window] #'minibuffer-keyboard-quit)

(keymap-set ivy-minibuffer-map "C-x C-s" #'ivy-occur)
(keymap-set ivy-minibuffer-map "M-r" #'ivy-reverse-i-search)
(keymap-set counsel-find-file-map "C-l" #'counsel-up-directory)

(evil-define-key 'insert minibuffer-mode-map
  (kbd "M-r") #'previous-matching-history-element)

(evil-define-key 'insert ivy-minibuffer-map
  (kbd "M-r") #'ivy-reverse-i-search)

(evil-define-key 'normal ivy-minibuffer-map
  (kbd "gg")  #'ivy-beginning-of-buffer
  (kbd "G")   #'ivy-end-of-buffer
  (kbd "C-d") #'ivy-scroll-up-command
  (kbd "C-u") #'ivy-scroll-down-command
  (kbd "C-o") #'hydra-ivy/body)

(evil-define-key '(insert normal) ivy-minibuffer-map
  (kbd "C-M-n") #'ivy-next-line-and-call
  (kbd "C-M-p") #'ivy-previous-line-and-call)

(defun init-ivy--action-append (x)
  "Append X after point."
  (unless (eolp) (forward-char))
  (ivy--action-insert x))

(ivy-add-actions t '(("a" init-ivy--action-append "append")))

(define-key counsel-mode-map [remap recentf-open] #'counsel-recentf)
(define-key counsel-mode-map [remap previous-matching-history-element] #'counsel-minibuffer-history)
(define-key counsel-mode-map [remap eshell-previous-matching-input] #'counsel-esh-history)
(define-key counsel-mode-map [remap comint-history-isearch-backward-regexp] #'counsel-shell-history)

;;; search

(setq! isearch-lazy-count t)
(setq! isearch-allow-scroll t)
(setq! isearch-allow-motion t)
(setq! isearch-yank-on-move t)
(setq! isearch-motion-changes-direction t)
(setq! isearch-repeat-on-direction-change t)

(keymap-global-set "C-s" #'swiper-thing-at-point)

(keymap-set search-map "s" #'swiper)
(keymap-set search-map "S" #'swiper-all)
(keymap-set search-map "/" #'swiper-from-isearch)

(keymap-set search-map "g" #'counsel-rg)
(keymap-set search-map "f" #'counsel-file-jump)
(keymap-set search-map "d" #'counsel-dired-jump)

(keymap-set swiper-isearch-map "TAB" #'swiper-isearch-toggle)
(keymap-set isearch-mode-map "TAB" #'swiper-isearch-toggle)

(defun init-after-swiper-isearch-forward (&rest _)
  "Reset `v/isearch-forward' after `swiper'."
  (setq isearch-forward t isearch-regexp t))

(defun init-after-swiper-isearch-backward (&rest _)
  "Reset `v/isearch-forward' after `swiper'."
  (setq isearch-forward nil isearch-regexp t))

(advice-add #'swiper-isearch :after #'init-after-swiper-isearch-forward)
(advice-add #'swiper-isearch-backward :after #'init-after-swiper-isearch-backward)

;;; goto

(require 'compile)

(keymap-set goto-map "M-g" #'goto-line)
(keymap-set goto-map "M-c" #'goto-char)

(keymap-set goto-map "c" #'compile)
(keymap-set goto-map "C" #'recompile)

(keymap-set goto-map "r" #'revert-buffer-quick)
(keymap-set goto-map "R" #'revert-buffer)
(keymap-set goto-map "v" #'vc-refresh-state)
(keymap-set goto-map "=" #'font-lock-update)

(keymap-set goto-map "<left>" #'previous-buffer)
(keymap-set goto-map "<right>" #'next-buffer)

(keymap-set goto-map "l" #'counsel-outline)

(setq! avy-style 'words)
(setq! avy-background t)
(setq! avy-all-windows nil)
(setq! avy-all-windows-alt t)
(setq! avy-single-candidate-jump nil)
(setq! avy-goto-word-0-regexp "\\_<\\(\\sw\\|\\s_\\)")

(require 'avy)

(avy-setup-default)

(keymap-global-set "C-'" #'avy-goto-char-timer)

(keymap-set goto-map ";" #'avy-resume)
(keymap-set goto-map "j" #'avy-goto-line)
(keymap-set goto-map "w" #'avy-goto-word-0)
(keymap-set goto-map "f" #'avy-goto-char-timer)

(setq! aw-dispatch-when-more-than 1)

(require 'ace-window)

(keymap-set goto-map "o" #'ace-window)

(ace-window-posframe-mode 1)

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

(keymap-unset help-map "t" t)
(keymap-set help-map "t l" #'load-library)
(keymap-set help-map "t f" #'load-file)
(keymap-set help-map "t t" #'load-theme)

(defun init-describe-symbol-at-point ()
  "Describe symbol at point."
  (interactive)
  (-if-let (symbol (symbol-at-point))
      (describe-symbol symbol)
    (user-error "No symbol at point")))

(setq! evil-lookup-func #'init-describe-symbol-at-point)

(defun init-lookup-setup-command (command)
  "Setup COMMAND as local help command."
  (setq-local evil-lookup-func command)
  (local-set-key [remap display-local-help] command))

(define-key counsel-mode-map [remap describe-bindings] nil t)

(defun init-counsel--set-variable (x)
  "Set variable X."
  (counsel-set-variable (intern x)))

(ivy-add-actions 'counsel-describe-variable '(("s" init-counsel--set-variable "set")))
(ivy-add-actions 'counsel-find-library '(("l" load-library "load")))

;;; project

(setq! projectile-current-project-on-switch 'move-to-end)

(require 'projectile)

(projectile-mode 1)

(keymap-set projectile-command-map "j" #'projectile-dired)
(keymap-set projectile-command-map "g" #'projectile-ripgrep)
(keymap-set projectile-command-map "s" #'projectile-save-project-buffers)
(keymap-set projectile-command-map "x" #'project-execute-extended-command)

(keymap-global-set "C-x p" projectile-command-map)
(keymap-global-set "C-x P" project-prefix-map)

(require 'counsel-projectile)

(define-key counsel-mode-map [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer)
(define-key counsel-mode-map [remap projectile-find-file] #'counsel-projectile-find-file)
(define-key counsel-mode-map [remap projectile-find-dir] #'counsel-projectile-find-dir)
(define-key counsel-mode-map [remap projectile-ripgrep] #'counsel-projectile-rg)

;;; vc

(require 'magit)

(defvar-keymap init-magit-command-map
  "v" #'magit-status
  "V" #'magit-dispatch
  "?" #'magit-file-dispatch
  "g" #'magit-status-here
  "G" #'magit-display-repository-buffer
  "s" #'magit-stage-buffer-file
  "u" #'magit-unstage-buffer-file
  "d" #'magit-diff-buffer-file
  "D" #'magit-diff
  "l" #'magit-log-buffer-file
  "L" #'magit-log
  "b" #'magit-blame-addition
  "B" #'magit-blame
  "f" #'magit-find-file
  "F" #'magit-blob-visit-file
  "n" #'magit-blob-next
  "p" #'magit-blob-previous)

(keymap-global-set "C-x v" init-magit-command-map)
(keymap-global-set "C-x V" vc-prefix-map)

;;; prog

;;;; eldoc

(require 'eldoc)

(setq! eldoc-minor-mode-string nil)

;;;; abbrev

(require 'abbrev)

(setq! only-global-abbrevs t)

(setq-default abbrev-mode t)

(init-diminish-minor-mode 'abbrev-mode)

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

(require 'ivy-yasnippet)

(define-key counsel-mode-map [remap yas-insert-snippet] #'ivy-yasnippet)

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

(define-key counsel-mode-map [remap company-search-candidates] #'counsel-company)

;;;; flycheck

(require 'flycheck)

(keymap-set flycheck-mode-map "M-n" #'flycheck-next-error)
(keymap-set flycheck-mode-map "M-p" #'flycheck-previous-error)

(add-hook 'prog-mode-hook #'flycheck-mode)

;;;; apheleia

(require 'apheleia)

(keymap-global-set "C-c =" #'apheleia-format-buffer)

;;;; lsp

(require 'lsp-mode)
(require 'lsp-ui)

(defun init-lookup-setup-lsp ()
  "Setup lsp ui doc."
  (init-lookup-setup-command #'lsp-ui-doc-glance))

(add-hook 'lsp-ui-mode-hook #'init-lookup-setup-lsp)

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

(require 'rg)

(keymap-set goto-map "g" #'rg-menu)
(keymap-set goto-map "d" #'rg-dwim)

;;;; diff

(setq! ediff-window-setup-function #'ediff-setup-windows-plain)

(require 'ediff)

;;;; comint

(require 'comint)

(evil-define-key 'insert comint-mode-map
  (kbd "M-r") #'comint-history-isearch-backward-regexp)

;;;; eshell

(require 'eshell)
(require 'em-prompt)
(require 'em-hist)

(defun init-eshell-set-outline ()
  "Set outline vars."
  (setq-local outline-regexp eshell-prompt-regexp)
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

(require 'eshell-dwim)

(keymap-set projectile-command-map "e" #'eshell-dwim-project)

;;;; spell

(setq! ispell-dictionary "american")

;;; lang

;;;; elisp

(dash-register-info-lookup)

(global-dash-fontify-mode 1)

(keymap-set emacs-lisp-mode-map "C-c e" #'macrostep-expand)
(keymap-set lisp-interaction-mode-map "C-c e" #'macrostep-expand)

(setq! flycheck-emacs-lisp-load-path load-path)

(require 'flycheck-package)

(flycheck-package-setup)

;;;; markdown

(setq! markdown-special-ctrl-a/e t)
(setq! markdown-fontify-code-blocks-natively t)

(require 'markdown-mode)
(require 'edit-indirect)

(keymap-set markdown-mode-map "C-c C-'" #'markdown-edit-code-block)
(keymap-set edit-indirect-mode-map "C-c C-'" #'edit-indirect-commit)

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

;;; minors

(defvar-keymap init-minor-map
  "a s" #'auto-save-visited-mode
  "a r" #'auto-revert-mode
  "f s" #'flyspell-mode
  "f c" #'flycheck-mode
  "r d" #'rainbow-delimiters-mode
  "r i" #'rainbow-identifiers-mode
  "t" #'toggle-truncate-lines
  "v" #'visual-line-mode
  "h" #'hl-line-mode
  "w" #'whitespace-mode
  "l" #'display-line-numbers-mode
  "L" #'init-toggle-line-numbers-type
  "s" #'lsp)

(keymap-global-set "C-x m" init-minor-map)

;;; leaders

(defvar init-leader-key "SPC")
(defvar init-leader-state '(motion normal visual operator))

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
 "SPC" #'ivy-switch-buffer
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
 "w" evil-window-map
 "4" ctl-x-4-map
 "5" ctl-x-5-map
 "t" tab-prefix-map)

(init-leader-global-set
 "b" #'switch-to-buffer
 "f" #'find-file
 "d" #'dired
 "j" #'dired-jump
 "k" #'kill-buffer
 "e" #'eshell-dwim
 "m" init-minor-map
 "r" ctl-x-r-map
 "h" help-map
 "g" goto-map
 "s" search-map
 "a" abbrev-map
 "n" narrow-map)

(init-leader-set markdown-mode-map
  "n b" #'markdown-narrow-to-block
  "n s" #'markdown-narrow-to-subtree)

(init-leader-set org-mode-map
  "n b" #'org-narrow-to-block
  "n s" #'org-narrow-to-subtree)

(init-leader-global-set
 "p" projectile-command-map
 "v" init-magit-command-map)

(init-leader-global-set
 "%" #'query-replace-regexp
 "=" #'apheleia-format-buffer
 "+" #'delete-trailing-whitespace
 "." #'xref-find-definitions
 "?" #'xref-find-references
 "," #'xref-go-back
 "i" #'imenu
 "l" #'counsel-outline
 "9" #'sp-wrap-round
 "(" #'init-sp-wrap-pair
 "[" #'init-sp-wrap-pair
 "{" #'init-sp-wrap-pair
 "<" #'init-sp-wrap-pair
 "'" #'init-sp-wrap-pair
 "`" #'init-sp-wrap-pair
 "\"" #'init-sp-wrap-pair)

(init-leader-minor-mode-set 'lsp-mode
  "y" lsp-command-map)

;;; end

(provide 'init-emacs)
;;; init-emacs.el ends here
