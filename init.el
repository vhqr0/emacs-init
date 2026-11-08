;;; init.el --- Init Emacs -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; My Emacs configuration.

;;; Code:

;;; essentials

(defvar init-directory (expand-file-name "emacs-init" user-emacs-directory))
(defvar priv-directory (expand-file-name "emacs-priv" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "lisp" init-directory))

(setq load-prefer-newer t)

(setq gc-cons-percentage 0.2)
(setq gc-cons-threshold (* 64 1024 1024))

(setq read-process-output-max (* 1024 1024))

(setq system-time-locale "C")

(prefer-coding-system 'utf-8)

;;; package

(require 'package)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; (setq package-quickstart t)

(defvar init-packages
  '(
    yasnippet
    company
    wgrep
    git-modes
    with-editor
    magit
    macrostep
    clojure-mode
    cider
    markdown-mode
    edit-indirect
    denote
    pyim
    pyim-basedict
    posframe
    ))

(when-let* ((packages (seq-remove #'package-installed-p init-packages)))
  (package-refresh-contents)
  (mapc #'package-install packages))

;;; vim

(require 'vim)

(vim-global-mode 1)

(defvar-keymap init-leader-map)

(keymap-set vim-normal-mode-map "SPC" init-leader-map)

(defun init-vim-merge (major modes map keys)
  "Set KEYS in the MODES override maps of major mode MAJOR as bound in MAP.
Each element of KEYS is either KEY or (FROM . KEY), where FROM is the key
in MAP; KEY alone is (KEY . KEY).  MODES is as in `vim-major-mode-map-set'."
  (let (bindings)
    (dolist (key keys)
      (let* ((key (if (consp key) key (cons key key)))
             (definition (keymap-lookup map (car key))))
        (when (and definition (not (numberp definition)))
          (setq bindings (nconc bindings (list (cdr key) definition))))))
    (apply #'vim-major-mode-map-set major modes bindings)))

(keymap-set vim-normal-mode-map "M-j" #'scroll-up-command)
(keymap-set vim-normal-mode-map "M-k" #'scroll-down-command)

;;; files

(defalias 'w 'save-buffer)

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

(defun init-auto-save-p ()
  "Predication of `auto-save-visited-mode'."
  (not vim-insert-mode))

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
;; (setq vc-display-status 'no-backend)
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
(require 'project-x)
(require 'project-test-jump)

(setq project-mode-line t)
(setq project-vc-merge-submodules nil)
(setq project-switch-use-entire-map t)
(setq project-compilation-buffer-name-function #'project-prefixed-buffer-name)

(keymap-set project-prefix-map "C" #'project-x-switch-to-compile)
(keymap-set project-prefix-map "t" #'project-test-jump)

;;;; ibuffer

(require 'ibuffer)

(defvar init-ibuffer-keys
  '(("n" . "j") ("p" . "k")
    "d" "D" "m" "M" "o" "O" "r" "R" "s" "S" "t" "T" "u" "U" "x" "X" "%" "=" "~")
  "Keys merged from `ibuffer-mode-map'.")

(vim-define-major-mode-map 'ibuffer-mode)

(init-vim-merge 'ibuffer-mode '(normal visual) ibuffer-mode-map init-ibuffer-keys)

;;; ui

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

(defun init-toggle-scroll-bar ()
  "Toggle scroll bar."
  (interactive)
  (if (or scroll-bar-mode horizontal-scroll-bar-mode)
      (progn
        (scroll-bar-mode -1)
        (horizontal-scroll-bar-mode -1))
    (scroll-bar-mode 1)
    (horizontal-scroll-bar-mode 1)))

(undelete-frame-mode 1)

(require 'tab-bar)

(setq tab-bar-position t)
;; (setq tab-bar-tab-hints t)
;; (setq tab-bar-select-tab-modifiers '(control meta))
(setq tab-bar-close-last-tab-choice 'delete-frame)
(setq tab-bar-history-limit 20)

(tab-bar-mode 1)
(tab-bar-history-mode 1)

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
(keymap-global-set "C-M-0" #'global-text-scale-adjust)
(keymap-global-set "C-M--" #'global-text-scale-adjust)
(keymap-global-set "C-M-+" #'global-text-scale-adjust)
(keymap-global-set "C-M-=" #'global-text-scale-adjust)

;;; edit

(setq ring-bell-function #'ignore)
(setq disabled-command-function nil)
(setq suggest-key-bindings nil)
(setq word-wrap-by-category t)
(setq save-interprogram-paste-before-kill t)
(setq kill-do-not-save-duplicates t)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)

(keymap-global-set "C-z" [escape])

(require 'hl-line)
(require 'display-line-numbers)

(defun init-hl-line-range ()
  "Return the current line range, or nil when the region is active."
  (unless (region-active-p)
    (cons (line-beginning-position) (line-beginning-position 2))))

(setq-default hl-line-range-function #'init-hl-line-range)

(defun init-toggle-trailing-whitespace ()
  "Toggle `show-trailing-whitespace'."
  (interactive)
  (setq-local show-trailing-whitespace (not show-trailing-whitespace)))

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

(require 'repeat)
(add-hook 'after-init-hook #'repeat-mode)

(setq isearch-lazy-count t)
(setq isearch-allow-scroll t)
(setq isearch-allow-motion t)
(setq isearch-yank-on-move t)
(setq isearch-motion-changes-direction t)
;; (setq isearch-repeat-on-direction-change t)

(require 'timestamp-at-point)

;;;; pair

(require 'elec-pair)
(electric-pair-mode 1)

(require 'paren)
;; (setq show-paren-style 'expression)
(setq show-paren-context-when-offscreen 'child-frame)
(show-paren-mode 1)

(define-advice show-paren--default (:around (func) vim)
  (if (not vim-normal-mode)
      (funcall func)
    (pcase (syntax-class (syntax-after (point)))
      (4 (save-restriction
           (narrow-to-region (point) (point-max))
           (funcall func)))
      (5 (save-excursion
           (forward-char 1)
           (save-restriction
             (narrow-to-region (point-min) (point))
             (funcall func)))))))

(require 'pair-x)
(keymap-global-set "C-<left>" #'pair-x-forward-barf-sexp)
(keymap-global-set "C-<right>" #'pair-x-forward-slurp-sexp)
(keymap-global-set "C-M-<left>" #'pair-x-backward-slurp-sexp)
(keymap-global-set "C-M-<right>" #'pair-x-backward-barf-sexp)
(keymap-set vim-normal-mode-map "M-r" #'raise-sexp)
(keymap-set vim-normal-mode-map "M-s" #'pair-x-splice-sexp)

(defun init-wrap-pair (&optional arg)
  "Insert pair, ARG see `insert-pair'."
  (interactive "*P")
  (insert-pair (or arg 1))
  (indent-sexp))

;;; minibuffer

(setq completion-styles '(flex))

(setq enable-recursive-minibuffers t)
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-extended-command-predicate #'command-completion-default-include-p)

(keymap-set minibuffer-local-map "<remap> <quit-window>" #'abort-recursive-edit)

(require 'savehist)

(savehist-mode 1)

(require 'icomplete)

(icomplete-mode 1)

(keymap-set icomplete-minibuffer-map "C-n" #'icomplete-forward-completions)
(keymap-set icomplete-minibuffer-map "C-p" #'icomplete-backward-completions)

(defun init-minibuffer-up-dir ()
  "Delete the last file name before point."
  (interactive)
  (when (and minibuffer-completing-file-name
             (> (point) (minibuffer-prompt-end)))
    ;; expand ~/
    (let ((path (buffer-substring-no-properties (minibuffer-prompt-end) (point))))
      (when (string-match-p "\\`~[^/]*/\\'" path)
        (delete-minibuffer-contents)
        (insert (expand-file-name path))))
    (save-excursion
      (let ((end (point)))
        (goto-char (1- end))
        (when (search-backward "/" (minibuffer-prompt-end) t)
          (delete-region (1+ (point)) end))))))

(keymap-set icomplete-minibuffer-map "C-l" #'init-minibuffer-up-dir)

(require 'iselect)

(add-hook 'iselect-buffer-mode-hook #'hl-line-mode)
(add-hook 'iselect-buffer-mode-hook #'display-line-numbers-mode)
(add-hook 'iselect-update-hook #'hl-line-highlight)
(add-hook 'iselect-move-hook #'hl-line-highlight)

(keymap-set iselect-minibuffer-map "<remap> <vim-j>" #'iselect-next)
(keymap-set iselect-minibuffer-map "<remap> <vim-k>" #'iselect-previous)
(keymap-set iselect-minibuffer-map "<remap> <vim-gg>" #'iselect-first)
(keymap-set iselect-minibuffer-map "<remap> <vim-G>" #'iselect-last)

(keymap-set icomplete-minibuffer-map "C-s" #'iselect-complete)

(keymap-set vim-insert-mode-map "M-r" #'iselect-history)

(require 'ifilter)

(add-hook 'ifilter-mode-hook #'hl-line-mode)
(add-hook 'ifilter-mode-hook #'display-line-numbers-mode)
(add-hook 'ifilter-line-goto-hook #'hl-line-highlight)

(require 'myswiper)

(add-hook 'myswiper-move-hook #'hl-line-highlight)

(keymap-set myswiper-map "<remap> <vim-j>" #'myswiper-next)
(keymap-set myswiper-map "<remap> <vim-k>" #'myswiper-previous)
(keymap-set myswiper-map "<remap> <vim-gg>" #'myswiper-first)
(keymap-set myswiper-map "<remap> <vim-G>" #'myswiper-last)

(require 'mygreper)

(add-hook 'mygreper-move-hook #'hl-line-highlight)

(keymap-set mygreper-map "<remap> <vim-j>" #'mygreper-next)
(keymap-set mygreper-map "<remap> <vim-k>" #'mygreper-previous)
(keymap-set mygreper-map "<remap> <vim-gg>" #'mygreper-first)
(keymap-set mygreper-map "<remap> <vim-G>" #'mygreper-last)

(defun init-search (&optional initial-input)
  "Search the buffer, or with prefix arg the project, with INITIAL-INPUT.
With two prefix args, read the directory to search."
  (interactive)
  (cond ((> (prefix-numeric-value current-prefix-arg) 4)
         (let ((default-directory (read-directory-name "Search directory: ")))
           (mygreper initial-input)))
        (current-prefix-arg
         (let ((default-directory (if-let* ((project (project-current)))
                                      (project-root project)
                                    default-directory)))
           (mygreper initial-input)))
        (t
         (myswiper initial-input))))

(defun init-search-at-point ()
  "Search the symbol at point, as `init-search'."
  (interactive)
  (init-search (when-let* ((symbol (thing-at-point 'symbol t)))
                 (regexp-quote symbol))))

(keymap-global-set "C-s" #'init-search-at-point)
(keymap-set search-map "s" #'init-search)

;;; outline

(require 'outline)
(require 'outline-x)

(setq outline-minor-mode-cycle t)
(setq outline-minor-mode-highlight 'override)
;; (setq outline-minor-mode-use-buttons 'in-margins)

(keymap-set narrow-map "s" #'outline-x-narrow-to-subtree)

;;; occur

(keymap-set occur-mode-map "C-c C-p" #'occur-edit-mode)

(require 'search-x)

(add-hook 'occur-mode-hook #'search-x-occur-header-setup)

(keymap-set occur-mode-map "/" #'search-x-occur-edit-regexp)
(keymap-set occur-mode-map "C-c C-/" #'search-x-occur-edit-regexp)

;;; dired

(require 'dired)
(require 'wdired)

(setq dired-dwim-target t)
(setq dired-auto-revert-buffer t)
(setq dired-listing-switches "-lha")

(put 'dired-jump 'repeat-map nil)

(keymap-set ctl-x-4-map "j" #'dired-jump-other-window)
(keymap-set project-prefix-map "j" #'project-dired)

(keymap-set dired-mode-map "C-c C-p" #'wdired-change-to-wdired-mode)

(defvar init-dired-keys
  '(("n" . "j") ("p" . "k")
    "c" "C" "d" "D" "m" "M" "o" "O" "r" "R" "s" "S" "t" "T" "u" "U" "x" "X" "%" "=" "~")
  "Keys merged from `dired-mode-map'.")

(vim-define-major-mode-map 'dired-mode)

(init-vim-merge 'dired-mode '(normal visual) dired-mode-map init-dired-keys)

(require 'arc-mode)

(defvar init-archive-keys
  '(("n" . "j") ("p" . "k")
    "c" "C" "d" "D" "m" "M" "o" "O" "r" "R" "u" "U" "x" "X")
  "Keys merged from `archive-mode-map'.")

(vim-define-major-mode-map 'archive-mode)

(init-vim-merge 'archive-mode '(normal visual) archive-mode-map init-archive-keys)

;;; image

(require 'image-mode)

(keymap-set image-mode-map "C-=" #'image-increase-size)
(keymap-set image-mode-map "C-+" #'image-increase-size)
(keymap-set image-mode-map "C--" #'image-decrease-size)

(keymap-set image-mode-map "M-n" #'image-next-file)
(keymap-set image-mode-map "M-p" #'image-previous-file)

(keymap-set image-mode-map "<remap> <vim-h>" #'image-backward-hscroll)
(keymap-set image-mode-map "<remap> <vim-j>" #'image-next-line)
(keymap-set image-mode-map "<remap> <vim-k>" #'image-previous-line)
(keymap-set image-mode-map "<remap> <vim-l>" #'image-forward-hscroll)
(keymap-set image-mode-map "<remap> <vim-gg>" #'image-bob)
(keymap-set image-mode-map "<remap> <vim-G>" #'image-eob)

(defvar init-image-keys
  '("m" "u")
  "Keys merged from `image-mode-map'.")

(vim-define-major-mode-map 'image-mode)

(init-vim-merge 'image-mode '(normal visual) image-mode-map init-image-keys)

;;; process

;;;; compile

(require 'compile)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(add-hook 'compilation-mode-hook #'search-x-compile-header-setup)

(keymap-set compilation-mode-map "/" #'search-x-compile-edit-command)
(keymap-set compilation-mode-map "C-c C-/" #'search-x-compile-edit-command)
(keymap-set compilation-minor-mode-map "/" #'search-x-compile-edit-command)
(keymap-set compilation-minor-mode-map "C-c C-/" #'search-x-compile-edit-command)

;;;; grep

(require 'grep)
(require 'wgrep)

(setq wgrep-auto-save-buffer t)
(setq wgrep-change-readonly-file t)

(require 'rg-dwim)

(defalias 'rg 'rg-dwim)

;;;; comint

(require 'comint)

(add-hook 'comint-mode-hook #'outline-x-comint-setup)

;;;; eshell

(require 'eshell)
(require 'em-cmpl)
(require 'em-alias)
(require 'eshell-dwim)

(setq eshell-aliases-file (expand-file-name "eshell-alias.esh" priv-directory))

(add-hook 'eshell-mode-hook #'outline-x-eshell-setup)

(keymap-unset eshell-cmpl-mode-map "C-M-i" t)

;;; vc

;;;; log view

(require 'log-view)

(defvar init-log-view-keys
  '("d" "D" "m" "u" "U" "=")
  "Keys merged from `log-view-mode-map'.")

(vim-define-major-mode-map 'log-view-mode)

(init-vim-merge 'log-view-mode '(normal visual) log-view-mode-map init-log-view-keys)

;;;; vc dir

(require 'vc-dir)

(defvar init-vc-dir-keys
  '(("n" . "j") ("p" . "k")
    "d" "D" "m" "M" "u" "U" "x" "o" "O" "I" ("P" . "p") "P" "%" "=")
  "Keys merged from `vc-dir-mode-map'.")

(vim-define-major-mode-map 'vc-dir-mode)

(init-vim-merge 'vc-dir-mode '(normal visual) vc-dir-mode-map init-vc-dir-keys)

;;;; ediff

(require 'ediff)

(setq ediff-window-setup-function #'ediff-setup-windows-plain)

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

(defun init-ediff-jump-to-last-difference ()
  "Jump to last difference."
  (interactive)
  (ediff-jump-to-difference -1))

(define-advice ediff-setup-keymap (:after () vim)
  (keymap-set ediff-mode-map "j" #'ediff-next-difference)
  (keymap-set ediff-mode-map "k" #'ediff-previous-difference)
  (keymap-set ediff-mode-map "g g" #'ediff-jump-to-difference)
  (keymap-set ediff-mode-map "G" #'init-ediff-jump-to-last-difference)
  (keymap-set ediff-mode-map "M-j" #'init-ediff-scroll-down)
  (keymap-set ediff-mode-map "M-k" #'init-ediff-scroll-up)
  (keymap-set ediff-mode-map "SPC" init-leader-map))

(defvar-keymap vim-ediff-mode-normal-override-map)
(defvar-keymap vim-ediff-mode-visual-override-map)
(defvar-keymap vim-ediff-mode-insert-override-map)

(setf (alist-get 'ediff-mode vim-major-mode-map-alist)
      (list (cons 'normal vim-ediff-mode-normal-override-map)
            (cons 'visual vim-ediff-mode-visual-override-map)
            (cons 'insert vim-ediff-mode-insert-override-map)))

(add-hook 'ediff-mode-hook #'vim-change-mode-to-default)

;;;; with editor

(require 'with-editor)

(shell-command-with-editor-mode 1)

(add-hook 'shell-mode-hook #'with-editor-export-editor)
(add-hook 'eshell-mode-hook #'with-editor-export-editor)

;;;; magit

(require 'magit)
(require 'magit-extras)

(keymap-set project-prefix-map "m" #'magit-project-status)

(keymap-set magit-mode-map "<remap> <quit-window>" #'magit-mode-bury-buffer)

(defvar init-magit-keys
  '("a" "A" "b" "B" "c" "C" "d" "D" "e" "E" "f" "F" "i" "I" "m"
    "o" "O" ("P" . "p") "P" "r" "R" "s" "S" "t" "T" "u" "U" "w" "W" "x" "X" "z")
  "Keys merged from `magit-mode-map'.")

(vim-define-major-mode-map 'magit-mode)

(init-vim-merge 'magit-mode '(normal visual) magit-mode-map init-magit-keys)

(vim-major-mode-map-set
 'magit-mode '(normal visual)
 "," #'magit-dispatch)

(keymap-set magit-blob-mode-map "<remap> <quit-window>" #'magit-kill-this-buffer)
(keymap-set magit-blob-mode-map "M-n" #'magit-blob-next)
(keymap-set magit-blob-mode-map "M-p" #'magit-blob-previous)

(keymap-set magit-blame-read-only-mode-map "<remap> <quit-window>" #'magit-blame-quit)
(keymap-set magit-blame-read-only-mode-map "M-n" #'magit-blame-next-chunk)
(keymap-set magit-blame-read-only-mode-map "M-p" #'magit-blame-previous-chunk)

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

(require 'simple-abbrev)

(setq simple-abbrev-file (expand-file-name "abbrevs.eld" priv-directory))

(add-hook 'after-init-hook #'simple-abbrev-load)

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
(setq company-frontends '(company-childframe-frontend company-preview-if-just-one-frontend company-echo-metadata-frontend))
(setq company-backends '(company-files company-capf (company-dabbrev-code company-keywords) company-dabbrev))

(keymap-unset company-active-map "M-n" t)
(keymap-unset company-active-map "M-p" t)
(keymap-set company-active-map "C-s" #'iselect-company)
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

(define-advice company-call-backend (:before-until (command &rest _) check-vim)
  (and (eq command 'prefix)
       (or vim-normal-mode vim-visual-mode)))

;;;; eldoc

(require 'eldoc)

(setq eldoc-minor-mode-string nil)
(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-echo-area-prefer-doc-buffer t)

(keymap-set prog-mode-map "<remap> <display-local-help>" #'eldoc-doc-buffer)

(defun init-eldoc-other-window ()
  "Switch to eldoc buffer other window."
  (interactive)
  (eldoc-print-current-symbol-info)
  (switch-to-buffer-other-window (eldoc-doc-buffer)))

;;;; flymake

(require 'flymake)
(require 'flymake-x)

(setq flymake-no-changes-timeout 1.0)
;; (setq flymake-show-diagnostics-at-end-of-line 'short)

(keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
(keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error)

;;;; format

(require 'simple-format)

;;;; xref

(require 'xref)

(setq xref-search-program 'ripgrep)

;;;; eglot

(require 'eglot)

(setq eglot-extend-to-xref t)

(keymap-set eglot-mode-map "<remap> <init-describe-symbol-dwim>" #'init-eldoc-other-window)

;;; elisp

;;;; lisp

(defun init-wrap-next-sexp-command (func)
  "Goto sexp end, then call last-sexp command FUNC."
  (save-excursion
    (when-let* ((end (cdr (bounds-of-thing-at-point 'sexp))))
      (goto-char end))
    (call-interactively func)))

(defmacro init-define-next-sexp-command (last-sexp-command)
  "Remap LAST-SEXP-COMMAND to next-sexp-command in vim normal mode."
  (let ((next-sexp-command (intern (concat "init-next-sexp@" (symbol-name last-sexp-command)))))
    `(prog1
         (defun ,next-sexp-command ()
           (interactive)
           (init-wrap-next-sexp-command ',last-sexp-command))
       (define-key vim-normal-mode-map [remap ,last-sexp-command] ',next-sexp-command))))

;;;; elisp

(init-define-next-sexp-command eval-last-sexp)
(init-define-next-sexp-command eval-print-last-sexp)
(init-define-next-sexp-command pp-eval-last-sexp)
(init-define-next-sexp-command pp-macroexpand-last-sexp)

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (keymap-set map "C-c C-d" #'checkdoc)
  (keymap-set map "C-c C-k" #'eval-buffer)
  (keymap-set map "C-c C-l" #'load-file)
  (keymap-set map "C-c C-m" #'pp-macroexpand-last-sexp))

(add-hook 'emacs-lisp-mode-hook #'outline-x-lisp-setup)

(require 'ielm)

(defun init-ielm-other-window ()
  "Switch to elisp repl other window."
  (interactive)
  (pop-to-buffer (get-buffer-create "*ielm*"))
  (ielm))

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (keymap-set map "C-c C-z" #'init-ielm-other-window))

(require 'macrostep)

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map inferior-emacs-lisp-mode-map))
  (keymap-set map "C-c e" #'macrostep-expand))

;;;; help

(setq help-window-select t)

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

(keymap-unset help-map "t" t)
(keymap-set help-map "t f" #'load-file)
(keymap-set help-map "t l" #'load-library)
(keymap-set help-map "t t" #'load-theme)

(defun init-describe-symbol-dwim ()
  "Describe symbol at point."
  (interactive)
  (describe-symbol (symbol-at-point)))

(keymap-set vim-normal-mode-map "K" #'init-describe-symbol-dwim)

;;; clojure

(require 'clojure-mode)

(add-hook 'clojure-mode-hook #'outline-x-lisp-setup)

(defun init-clojure-set-elec-pairs ()
  "Set `electric-pair-pairs' for Clojure mode."
  (setq-local electric-pair-pairs
              (add-to-list 'electric-pair-pairs '(?` . ?`))))

(add-hook 'clojure-mode-hook #'init-clojure-set-elec-pairs)

(defun init-clojure-remove-comma-dwim ()
  "Remove comma dwim."
  (interactive)
  (let ((bounds (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (bounds-of-thing-at-point 'sexp))))
    (replace-string-in-region "," "" (car bounds) (cdr bounds))))

(keymap-set clojure-refactor-map "," #'init-clojure-remove-comma-dwim)

(add-hook 'clojure-mode-hook #'flymake-x-setup)

;;;; cider

(require 'cider)
(require 'cider-format)
(require 'cider-macroexpansion)

(setq cider-mode-line '(:eval (format " Cider[%s]" (cider--modeline-info))))

(init-define-next-sexp-command cider-eval-last-sexp)
(init-define-next-sexp-command cider-eval-last-sexp-to-repl)
(init-define-next-sexp-command cider-eval-last-sexp-in-context)
(init-define-next-sexp-command cider-eval-last-sexp-and-replace)
(init-define-next-sexp-command cider-pprint-eval-last-sexp)
(init-define-next-sexp-command cider-pprint-eval-last-sexp-to-repl)
(init-define-next-sexp-command cider-pprint-eval-last-sexp-to-comment)
(init-define-next-sexp-command cider-insert-last-sexp-in-repl)
(init-define-next-sexp-command cider-tap-last-sexp)
(init-define-next-sexp-command cider-format-edn-last-sexp)
(init-define-next-sexp-command cider-inspect-last-sexp)
(init-define-next-sexp-command cider-macroexpand-1)
(init-define-next-sexp-command cider-macroexpand-all)
(init-define-next-sexp-command cider-macroexpand-1-inplace)
(init-define-next-sexp-command cider-macroexpand-all-inplace)

(keymap-set cider-mode-map "C-c C-n" #'cider-repl-set-ns)
(keymap-set cider-mode-map "C-c C-i" #'cider-insert-last-sexp-in-repl)
(keymap-set cider-mode-map "C-c C-;" #'cider-pprint-eval-last-sexp-to-comment)

(add-to-list 'vim-eval-function-alist '(clojure-mode . cider-eval-region))

(dolist (map (list cider-mode-map cider-repl-mode-map))
  (keymap-set map "C-M-q" #'cider-format-edn-last-sexp)
  (keymap-set map "<remap> <init-describe-symbol-dwim>" #'cider-doc))

(add-to-list 'iselect-history-alist
             '(cider-repl-mode cider-repl-input-history
                               cider-repl-input-history-position
                               cider-repl-input-start-mark))

(defun init-cider-repl-set-xref ()
  "Set Xref backend for Cider REPL."
  (add-hook 'xref-backend-functions #'cider--xref-backend nil t))

(add-hook 'cider-repl-mode-hook #'init-cider-repl-set-xref)

;;;;; macrostep

(require 'macrostep-cider)

(dolist (hook '(cider-mode-hook cider-repl-mode-hook))
  (add-hook hook #'macrostep-cider-setup))

(dolist (map (list cider-mode-map cider-repl-mode-map))
  (keymap-set map "C-c e" #'macrostep-expand))

;;; python

(require 'python)

(setq python-shell-interpreter "python")
(setq python-shell-interpreter-args "-m IPython --simple-prompt")

(keymap-set python-base-mode-map "C-c C-k" #'python-shell-send-buffer)

(add-to-list 'vim-eval-function-alist '(python-mode . python-shell-send-region))

;;; markdown

(require 'markdown-mode)
(require 'markdown-org)

(setq markdown-special-ctrl-a/e t)
(setq markdown-fontify-code-blocks-natively t)

(setq markdown-org-capture-major-mode #'markdown-mode)

(defun init-capture-time ()
  "Return the current time for capture templates."
  (format-time-string "%F %a %R"))

(defun init-capture-location ()
  "Return the file and line where capture started."
  (format "%s %d"
          (or markdown-org-capture-origin-file
              (buffer-name markdown-org-capture-origin-buffer))
          markdown-org-capture-origin-line))

(defun init-capture-context ()
  "Return the region or the line where capture started."
  (string-trim-right
   (or markdown-org-capture-origin-region markdown-org-capture-origin-line-text)
   "\n+"))

(defun init-capture-initial ()
  "Return the region where capture started or the last kill."
  (string-trim-right
   (or markdown-org-capture-origin-region (ignore-errors (current-kill 0 t)) "")
   "\n+"))

(setq markdown-org-capture-templates
      `((?t "Todo" ,markdown-org-capture-default-file
            "# TODO $0\n`(init-capture-time)`\n")
        (?a "Todo With Context" ,markdown-org-capture-default-file
            "# TODO $0\n`(init-capture-time)`\n`(init-capture-location)`\n\\`\\`\\`\n`(init-capture-context)`\n\\`\\`\\`\n")
        (?i "Todo With Initial Content" ,markdown-org-capture-default-file
            "# TODO $0\n`(init-capture-time)`\n`(init-capture-initial)`\n")))

(keymap-set markdown-mode-map "C-c C-t" #'markdown-org-toggle-todo)

(defvar init-markdown-org-agenda-keys
  '("o")
  "Keys merged from `markdown-org-agenda-mode-map'.")

(vim-define-major-mode-map 'markdown-org-agenda-mode)

(init-vim-merge 'markdown-org-agenda-mode '(normal visual) markdown-org-agenda-mode-map init-markdown-org-agenda-keys)

;;;; denote

(require 'denote)

(setq denote-directory (expand-file-name "denote" priv-directory))
(setq denote-file-type 'markdown-yaml)

;;; input method

(keymap-global-set "C-SPC" #'toggle-input-method)
(keymap-global-set "C-@" #'toggle-input-method)
(keymap-set isearch-mode-map "C-SPC" #'isearch-toggle-input-method)
(keymap-set isearch-mode-map "C-@" #'isearch-toggle-input-method)

(defun init-ignore-input-method-p ()
  "Predicate of input method."
  (and (not isearch-mode) (or vim-normal-mode vim-visual-mode)))

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

;;;; pyim

(require 'posframe)
(require 'pyim)
(require 'pyim-basedict)
(require 'pyim-zirjma)

(setq default-input-method "pyim")

(setq pyim-default-scheme 'zirjma)
(setq pyim-pinyin-fuzzy-alist nil)
(setq pyim-enable-shortcode nil)
(setq pyim-candidates-search-buffer-p nil)
(setq pyim-indicator-list nil)
(setq pyim-page-tooltip '(posframe))

(setq pyim-punctuation-dict
      '(("'"  "‘"  "’")
        ("\"" "“"  "”")
        ("^"  "…"     )
        ("$"  "¥"     )
        ("("  "（"    )
        (")"  "）"    )
        ("["  "【"    )
        ("]"  "】"    )
        ("{"  "「"    )
        ("}"  "」"    )
        ("<"  "《"    )
        (">"  "》"    )
        ("?"  "？"    )
        ("!"  "！"    )
        (","  "，"    )
        ("."  "。"    )
        (";"  "；"    )
        (":"  "："    )
        ("\\" "、"    )))

(keymap-set pyim-mode-map "." #'pyim-page-next-page)
(keymap-set pyim-mode-map "," #'pyim-page-previous-page)

(add-hook 'after-init-hook #'pyim-basedict-enable)

(advice-add #'pyim-input-method :around #'init-wrap-input-method)

;;; leaders

(defun init-leader-set (&rest clauses)
  "Set leader binding CLAUSES in `init-leader-map'."
  (dolist (binding (seq-partition clauses 2))
    (keymap-set init-leader-map (car binding) (cadr binding))))

(defun init-magic-prefix (prefix)
  "Magically read and execute command on PREFIX."
  (let ((event (read-event (concat prefix " C-"))))
    (if (eq event ?\C-h)
        (describe-keymap (key-binding (kbd prefix)))
      (let ((literal (eq event ?\s))
            event-string new-prefix binding)
        (when literal
          (setq event (read-event prefix)))
        (setq event-string (if (symbolp event)
                               (concat "<" (symbol-name event) ">")
                             (char-to-string event)))
        (unless literal
          (setq new-prefix (concat prefix " C-" event-string)
                binding (key-binding (kbd new-prefix))))
        (unless binding
          (setq new-prefix (concat prefix " " event-string)
                binding (key-binding (kbd new-prefix))))
        (cond ((and binding (commandp binding))
               (setq this-command binding
                     real-this-command binding
                     last-command-event event)
               (if (commandp binding t)
                   (call-interactively binding)
                 (execute-kbd-macro binding)))
              ((and binding (keymapp binding))
               (init-magic-prefix new-prefix))
              (t
               (user-error "No magic key binding found on %s %s" prefix event-string)))))))

(defun init-magic-C-x ()
  "Magic control C."
  (interactive)
  (init-magic-prefix "C-x"))

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

(keymap-set vim-normal-mode-map "C-p" #'iselect-project-files)

(init-leader-set
 "SPC" #'iselect-files
 "\\" #'init-magic-shift
 "TAB" #'init-magic-shift
 "x" #'init-magic-C-x
 "c" #'init-magic-C-c
 "u" #'init-magic-C-u
 "z" #'repeat
 "y" #'ifilter-kill-ring
 "i" #'ifilter-imenu
 "l" #'ifilter-outline
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
 "C" #'markdown-org-capture
 "A" #'markdown-org-agenda-todo
 "w" window-prefix-map
 "4" ctl-x-4-map
 "5" ctl-x-5-map
 "t" tab-prefix-map
 "p" project-prefix-map
 "v" vc-prefix-map
 "r" ctl-x-r-map
 "h" help-map
 "g" goto-map
 "s" search-map
 "n" narrow-map
 "a" abbrev-map
 "m" init-minor-prefix-map
 "T" #'timestamp-at-point
 "e" #'eshell-dwim
 "S" #'rg-dwim
 "O" #'search-x-occur-at-point
 "Q" #'search-x-query-replace-at-point
 "%" #'query-replace-regexp
 "$" #'ispell-word
 "=" #'simple-format-buffer
 "+" #'delete-trailing-whitespace
 "." #'xref-find-definitions
 "?" #'xref-find-references
 "," #'xref-go-back
 "(" #'init-wrap-pair
 "[" #'init-wrap-pair
 "{" #'init-wrap-pair
 "<" #'init-wrap-pair
 "'" #'init-wrap-pair
 "`" #'init-wrap-pair
 "\"" #'init-wrap-pair)

;;; end

(provide 'init)
;;; init.el ends here
