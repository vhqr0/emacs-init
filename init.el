;;; init.el --- Init Emacs -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; My Emacs configuration.

;;; Code:

;;; essentials

(defvar init-directory (expand-file-name "emacs-init" user-emacs-directory))
(defvar priv-directory (expand-file-name "emacs-priv" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "vim.el" init-directory))

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
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

;; (setq package-quickstart t)

(defvar init-packages
  '(
    paredit
    avy
    hydra
    amx
    ivy
    ivy-avy
    ivy-hydra
    swiper
    counsel
    yasnippet
    company
    apheleia
    wgrep
    git-modes
    with-editor
    magit
    orgit
    macrostep
    clojure-mode
    cider
    org-roam
    markdown-mode
    edit-indirect
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

(keymap-set vim-normal-mode-map "SPC" init-leader-map)
(keymap-set vim-normal-mode-map "<remap> <scroll-up-command>" #'init-leader-or-scroll-up-command)

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

(defvar-keymap init-header-revert-keymap
  "<header-line> <mouse-1>" #'revert-buffer)

(defvar init-revert-header-line-format
  (propertize
   "Revert"
   'face 'mode-line-buffer-id
   'mouse-face 'mode-line-highlight
   'local-map init-header-revert-keymap))

(keymap-set ctl-x-x-map "<left>" #'previous-buffer)
(keymap-set ctl-x-x-map "<right>" #'next-buffer)

(defun init-kill-current-buffer ()
  "Confirm then kill current buffer."
  (interactive)
  (when (y-or-n-p "Kill current buffer?")
    (kill-current-buffer)))

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

(setq project-mode-line t)
(setq project-switch-use-entire-map t)
(setq project-compilation-buffer-name-function #'project-prefixed-buffer-name)
(setq project-vc-merge-submodules nil)

(defun init-project-switch-to-compile ()
  "Switch to project compilation buffer."
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (if-let* ((buffer (get-buffer (compilation-buffer-name "compilation" nil nil))))
        (switch-to-buffer buffer)
      (user-error "No project compilation buffer found"))))

(keymap-set project-prefix-map "C" #'init-project-switch-to-compile)

(defvar-local init-find-test-file-function nil)

(defun init-project-find-test-file ()
  "Find test file in this project."
  (interactive)
  (if (not init-find-test-file-function)
      (user-error "No find test file function found")
    (let ((default-directory (project-root (project-current t))))
      (funcall init-find-test-file-function))))

(keymap-set project-prefix-map "t" #'init-project-find-test-file)

;;;; ibuffer

(require 'ibuffer)

(defvar init-ibuffer-keys
  '(("n" . "j") ("p" . "k")
    "d" "D" "m" "o" "O" "s" "S" "t" "u" "U" "x" "%")
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

(keymap-set tab-prefix-map "<left>" #'tab-bar-history-back)
(keymap-set tab-prefix-map "<right>" #'tab-bar-history-forward)

(keymap-set window-prefix-map "<left>" #'tab-bar-history-back)
(keymap-set window-prefix-map "<right>" #'tab-bar-history-forward)

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

(defun init-occur-at-point ()
  "Occur thing at point."
  (interactive)
  (occur (regexp-quote (thing-at-point 'symbol))))

(defun init-query-replace-at-point ()
  "Query replace at point."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (from (buffer-substring (car bounds) (cdr bounds))))
    (goto-char (car bounds))
    (query-replace
     from (query-replace-read-to from "Query replace" nil))))

(require 'avy)
(setq avy-background t)
(keymap-global-set "C-'" #'avy-goto-char-timer)
(keymap-set goto-map "j" #'avy-goto-line-below)
(keymap-set goto-map "k" #'avy-goto-line-above)

(defun init-convert-timestamp-dwim (ts)
  "Convert TS to time string dwim.
Support:
- seconds and milliseconds duration in one day.
- seconds and milliseconds posix timestamp from 2001 to 2286."
  (cond
   ((<= 90 ts 86400)
    (format "%02d:%02d:%02d"
            (/ ts 3600)
            (mod (/ ts 60) 60)
            (mod ts 60)))
   ((<= 90000 ts 86400000)
    (format "%02d:%02d:%02d:%03d"
            (/ ts 3600000)
            (mod (/ ts 60000) 60)
            (mod (/ ts 1000) 60)
            (mod ts 1000)))
   ((<= 1000000000 ts 9999999999)
    (format-time-string "%Z %Y-%m-%d %H:%M:%S" ts))
   ((<= 1000000000000 ts 9999999999999)
    (let ((ts (list 0 (/ ts 1000) (* 1000 (% ts 1000)) 0)))
      (format-time-string "%Z %Y-%m-%d %H:%M:%S:%3N" ts)))))

(defun init-echo-timestamp-dwim ()
  "Echo timestamp at point dwim."
  (interactive)
  (if-let* ((ts (thing-at-point 'number)))
      (if-let* ((s (init-convert-timestamp-dwim ts)))
          (progn
            (kill-new s)
            (message s))
        (user-error "Not a timestamp"))
    (user-error "No number at point")))

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
(keymap-set vim-normal-mode-map "M-r" #'raise-sexp)
(keymap-set vim-normal-mode-map "M-s" #'paredit-splice-sexp)

(defun init-wrap-pair (&optional arg)
  "Insert pair, ARG see `insert-pair'."
  (interactive "*P")
  (insert-pair (or arg 1))
  (indent-sexp))

;;; minibuffer

(setq enable-recursive-minibuffers t)
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-extended-command-predicate #'command-completion-default-include-p)

(keymap-set minibuffer-local-map "<remap> <quit-window>" #'abort-recursive-edit)

(require 'savehist)

(savehist-mode 1)

;;;; ivy

(require 'amx)
(require 'ivy)
(require 'ivy-hydra)
(require 'ivy-avy)
(require 'swiper)
(require 'counsel)

(setq ivy-count-format "(%d/%d) ")
(setq ivy-use-virtual-buffers t)

(setcdr (assq 'ivy-mode minor-mode-alist) '(""))
(setcdr (assq 'counsel-mode minor-mode-alist) '(""))

(amx-mode 1)
(ivy-mode 1)
(counsel-mode 1)

(keymap-set ivy-mode-map "C-c b" #'ivy-resume)

(keymap-set ivy-minibuffer-map "<remap> <save-buffer>" #'ivy-occur)
(keymap-set ivy-minibuffer-map "<remap> <quit-window>" #'abort-recursive-edit)
(keymap-set ivy-minibuffer-map "<remap> <vim-j>" #'ivy-next-line)
(keymap-set ivy-minibuffer-map "<remap> <vim-k>" #'ivy-previous-line)
(keymap-set ivy-minibuffer-map "<remap> <vim-gg>" #'ivy-beginning-of-buffer)
(keymap-set ivy-minibuffer-map "<remap> <vim-G>" #'ivy-end-of-buffer)

(defun init-ivy-up-directory ()
  "Ivy up directory."
  (interactive)
  (when ivy--directory
    (funcall #'counsel-up-directory)))

(keymap-set ivy-minibuffer-map "C-l" #'init-ivy-up-directory)

(keymap-set ivy-occur-mode-map "<remap> <revert-buffer-quick>" #'ivy-occur-revert-buffer)
(keymap-set ivy-occur-mode-map "<remap> <revert-buffer>" #'ivy-occur-revert-buffer)

(keymap-set counsel-mode-map "<remap> <recentf-open>" #'counsel-recentf)
(keymap-set counsel-mode-map "<remap> <company-search-candidates>" #'counsel-company)

(defun init-search (&optional initial-input)
  "Search things dwim with optional INITIAL-INPUT."
  (interactive)
  (let ((arg (prefix-numeric-value current-prefix-arg)))
    (if (>= arg 4)
        (progn
          (setq current-prefix-arg (- arg 4))
          (counsel-rg initial-input))
      (swiper initial-input))))

(defun init-search-at-point ()
  "Search thing at point."
  (interactive)
  (init-search (thing-at-point 'symbol)))

(keymap-global-set "C-s" #'init-search-at-point)
(keymap-set search-map "s" #'init-search)

(defun init-history-placeholder ()
  "Search history command placeholder."
  (interactive)
  (user-error "No history command available"))

(keymap-set vim-insert-mode-map "M-r" #'init-history-placeholder)

(keymap-set ivy-minibuffer-map "<remap> <init-history-placeholder>" #'ivy-reverse-i-search)
(keymap-set minibuffer-local-map "<remap> <init-history-placeholder>" #'counsel-minibuffer-history)

;;; outline

(require 'outline)

(setq outline-minor-mode-cycle t)
(setq outline-minor-mode-highlight 'override)
;; (setq outline-minor-mode-use-buttons 'in-margins)

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

(keymap-set occur-mode-map "C-c C-p" #'occur-edit-mode)

(defun init-occur-edit-regexp ()
  "Edit occur regexp."
  (interactive)
  (let ((regexp (read-string "Occur regexp: " (car occur-revert-arguments) 'regexp-history)))
    (setf (car occur-revert-arguments) regexp))
  (occur-revert-function nil nil))

(defun init-occur-edit-buffer ()
  "Edit occur buffer."
  (interactive)
  (let ((buffer (get-buffer (read-buffer "Occur buffer: " nil t))))
    (setq default-directory (buffer-local-value 'default-directory buffer))
    (setq occur-revert-arguments (list (car occur-revert-arguments) nil (list buffer))))
  (occur-revert-function nil nil))

(defvar-keymap init-occur-header-edit-regexp-keymap
  "<header-line> <mouse-1>" #'init-occur-edit-regexp)

(defvar-keymap init-occur-header-edit-buffer-keymap
  "<header-line> <mouse-1>" #'init-occur-edit-buffer)

(defvar init-occur-header-line-format
  (concat
   init-revert-header-line-format
   " "
   (propertize
    "EditRegexp"
    'face 'mode-line-buffer-id
    'mouse-face 'mode-line-highlight
    'local-map init-occur-header-edit-regexp-keymap)
   " "
   (propertize
    "EditBuffer"
    'face 'mode-line-buffer-id
    'mouse-face 'mode-line-highlight
    'local-map init-occur-header-edit-buffer-keymap)))

(defun init-occur-set-header ()
  "Set header."
  (setq header-line-format init-occur-header-line-format))

(add-hook 'occur-mode-hook #'init-occur-set-header)

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
    "c" "C" "d" "D" "m" "o" "O" "r" "R" "s" "S" "t" "T" "u" "U" "x" "X" "%" "=" "~")
  "Keys merged from `dired-mode-map'.")

(vim-define-major-mode-map 'dired-mode)

(init-vim-merge 'dired-mode '(normal visual) dired-mode-map init-dired-keys)

(require 'arc-mode)

(defvar init-archive-keys
  '(("n" . "j") ("p" . "k")
    "C" "m" "o" "u")
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

(defun init-compile-edit-command ()
  "Edit compile command."
  (interactive)
  (recompile t))

(defun init-compile-edit-directory ()
  "Edit compile directory."
  (interactive)
  (let ((directory (read-directory-name "Compile directory: ")))
    (setq default-directory directory)
    (setq compilation-directory directory))
  (apply #'compilation-start compilation-arguments))

(defvar-keymap init-compile-header-edit-command-keymap
  "<header-line> <mouse-1>" #'init-compile-edit-command)

(defvar-keymap init-compile-header-edit-directory-keymap
  "<header-line> <mouse-1>" #'init-compile-edit-directory)

(defvar init-compile-header-line-format
  (concat
   init-revert-header-line-format
   " "
   (propertize
    "EditCommand"
    'face 'mode-line-buffer-id
    'mouse-face 'mode-line-highlight
    'local-map init-compile-header-edit-command-keymap)
   " "
   (propertize
    "EditDirectory"
    'face 'mode-line-buffer-id
    'mouse-face 'mode-line-highlight
    'local-map init-compile-header-edit-directory-keymap)))

(defun init-compile-set-header ()
  "Set header."
  (setq header-line-format init-compile-header-line-format))

(add-hook 'compilation-mode-hook #'init-compile-set-header)

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

(keymap-set comint-mode-map "<remap> <init-history-placeholder>" #'counsel-shell-history)

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

(keymap-set eshell-mode-map "<remap> <init-history-placeholder>" #'counsel-esh-history)

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

(defun init-eshell-dwim-switch-to-buffer-split-window (buffer)
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

(defun init-eshell-dwim (&optional arg)
  "Do open eshell smartly.
Without universal ARG, open in split window.
With universal ARG, open in other window.
With two universal ARG, open in this window."
  (interactive "P")
  (let ((buffer (init-eshell-dwim-get-buffer-create)))
    (cond ((> (prefix-numeric-value arg) 4)
           (switch-to-buffer buffer))
          (arg
           (switch-to-buffer-other-window buffer))
          (t
           (init-eshell-dwim-switch-to-buffer-split-window buffer)))))

;;; vc

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
(setq company-frontends '(company-childframe-frontend company-preview-if-just-one-frontend company-echo-metadata-frontend))
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

(setq flymake-no-changes-timeout 1.0)
;; (setq flymake-show-diagnostics-at-end-of-line 'short)

(keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
(keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error)

(defvar-local init-flymake-make-command-function nil)
(defvar-local init-flymake-make-report-function nil)

(defvar-local init-flymake-proc nil)

(defun init-flymake-make-proc (buffer report-fn)
  "Make Flymake process for BUFFER.
REPORT-FN see `init-flymake-backend'."
  (when-let* ((make-command-function (buffer-local-value 'init-flymake-make-command-function buffer)))
    (when-let* ((command (funcall make-command-function)))
      (let* ((proc-buffer-name (format "*init-flymake for %s*" (buffer-name buffer)))
             (sentinel
              (lambda (proc _event)
                (when (memq (process-status proc) '(exit signal))
                  (let ((proc-buffer (process-buffer proc)))
                    (unwind-protect
                        (if (eq proc (buffer-local-value 'init-flymake-proc buffer))
                            (let ((make-report-function (buffer-local-value 'init-flymake-make-report-function buffer)))
                              (with-current-buffer buffer
                                (save-excursion
                                  (save-restriction
                                    (widen)
                                    (with-current-buffer proc-buffer
                                      (widen)
                                      (goto-char (point-min))
                                      (funcall report-fn (funcall make-report-function buffer)))))))
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
  (when-let* ((proc (init-flymake-make-proc (current-buffer) report-fn)))
    (when (process-live-p init-flymake-proc)
      (kill-process init-flymake-proc))
    (setq init-flymake-proc proc)
    (save-restriction
      (widen)
      (process-send-region proc (point-min) (point-max))
      (process-send-eof proc))))

;;;; xref

(require 'xref)

(setq xref-search-program 'ripgrep)

;;;; eglot

(require 'eglot)

(setq eglot-extend-to-xref t)

(keymap-set eglot-mode-map "<remap> <init-describe-symbol-dwim>" #'init-eldoc-other-window)

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

(add-hook 'emacs-lisp-mode-hook #'init-lisp-set-outline)

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

(add-hook 'clojure-mode-hook #'init-lisp-set-outline)

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

;;;; test

(defvar init-clojure-extensions '("cljc" "clj" "cljs"))

(defun init-clojure-extensions (extension)
  "Return clojure file extensions, given EXTENSION first."
  (cons extension (remove extension init-clojure-extensions)))

;; (init-clojure-extensions "clj") => '("clj" "cljc" "cljs")
;; (init-clojure-extensions "cljc") => '("cljc" "clj" "cljs")

(defun init-clojure-file-with-extensions (file)
  "Return clojure files with different extension, given FILE first."
  (let ((base (file-name-sans-extension file))
        (extension (file-name-extension file)))
    (thread-last
      (init-clojure-extensions extension)
      (seq-map (lambda (extension) (concat base "." extension))))))

;; (init-clojure-file-with-extensions "foo/bar.clj")
;; => '("foo/bar.clj" "foo/bar.cljc" "foo/bar.cljs")
;; (init-clojure-file-with-extensions "foo/bar.cljc")
;; => '("foo/bar.cljc" "foo/bar.clj" "foo/bar.cljs")

(defun init-clojure-test-file (file)
  "Convert FILE to test file with same extension."
  (let ((file (concat "/" file)))
    (cond
     ((string-match "\\(.*?\\)/src/\\(.*\\)\\(\\.clj.?\\)$" file)
      (thread-first
        (concat (match-string 1 file) "/test/" (match-string 2 file) "_test" (match-string 3 file))
        (substring 1)))
     ((string-match "\\(.*?\\)/test/\\(.*\\)_test\\(\\.clj.?\\)$" file)
      (thread-first
        (concat (match-string 1 file) "/src/" (match-string 2 file) (match-string 3 file))
        (substring 1))))))

;; (init-clojure-test-file "src/foo/bar.clj") => "test/foo/bar_test.clj"
;; (init-clojure-test-file "test/foo/bar_test.clj") => "src/foo/bar.clj"
;; (init-clojure-test-file "clojure/src/foo/bar.clj") => "clojure/test/foo/bar_test.clj"
;; (init-clojure-test-file "clojure/test/foo/bar_test.clj") => "clojure/src/foo/bar.clj"

(defun init-clojure-test-files (file)
  "Convert FILE to test files, with possible extensions."
  (when-let* ((file (init-clojure-test-file file)))
    (init-clojure-file-with-extensions file)))

;; (init-clojure-test-files "src/foo/bar.clj")
;; => '("test/foo/bar_test.clj" "test/foo/bar_test.cljc" "test/foo/bar_test.cljs")
;; (init-clojure-test-files "test/foo/bar_test.cljc")
;; => '("src/foo/bar.cljc" "src/foo/bar.clj" "src/foo/bar.cljs")

(defun init-clojure-find-test-file ()
  "Find test file of current buffer."
  (if (not buffer-file-name)
      (user-error "No buffer file name found")
    (let* ((file (file-relative-name buffer-file-name))
           (files (init-clojure-test-files file)))
      (if-let* ((file (seq-find #'file-exists-p files)))
          (find-file file)
        (if-let* ((file (car files)))
            (find-file (read-file-name
                        "Create test file: "
                        (file-name-directory file)
                        nil nil
                        (file-name-nondirectory file)))
          (user-error "No test file found"))))))

(defun init-clojure-set-find-test-file ()
  "Set `init-find-test-file' for Clojure mode."
  (setq-local init-find-test-file-function #'init-clojure-find-test-file))

(add-hook 'clojure-mode-hook #'init-clojure-set-find-test-file)

;;;; kondo

(defvar init-clojure-kondo-program "clj-kondo")

(defun init-clojure-kondo-make-command ()
  "Make kondo command."
  (when (executable-find init-clojure-kondo-program)
    (let* ((buffer-file-name (buffer-file-name))
           (lang (if (not buffer-file-name)
                     "clj"
                   (file-name-extension buffer-file-name))))
      `(,init-clojure-kondo-program
        "--lint" "-"
        "--lang" ,lang
        ,@(when buffer-file-name
            `("--filename" ,buffer-file-name))))))

(defconst init-clojure-kondo-diag-regexp
  "^.+:\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\([[:alpha:]]+\\): \\(.+\\)$")

(defvar init-clojure-kondo-type-alist
  '(("error" . :error) ("warning" . :warning)))

(defun init-clojure-kondo-make-report (buffer)
  "Make flymake report for kondo in source BUFFER."
  (let (diags)
    (while (search-forward-regexp init-clojure-kondo-diag-regexp nil t)
      (let* ((row (string-to-number (match-string 1)))
             (col (string-to-number (match-string 2)))
             (type (or (cdr (assoc (match-string 3) init-clojure-kondo-type-alist)) :type))
             (msg (match-string 4))
             (region (flymake-diag-region buffer row col))
             (diag (flymake-make-diagnostic buffer (car region) (cdr region) type msg)))
        (push diag diags)))
    (nreverse diags)))

(defun init-clojure-set-kondo ()
  "Set kondo Flymake backend."
  (setq-local init-flymake-make-command-function #'init-clojure-kondo-make-command)
  (setq-local init-flymake-make-report-function #'init-clojure-kondo-make-report)
  (add-hook 'flymake-diagnostic-functions #'init-flymake-backend nil t))

(add-hook 'clojure-mode-hook #'init-clojure-set-kondo)

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

(defun init-counsel-cider-repl-history ()
  "Browse Cider REPL history."
  (interactive)
  (setq ivy-completion-beg (point))
  (setq ivy-completion-end (point))
  (ivy-read "History: " (ivy-history-contents cider-repl-input-history)
            :keymap ivy-reverse-i-search-map
            :action #'counsel--browse-history-action
            :caller #'init-counsel-cider-repl-history))

(dolist (map (list cider-mode-map cider-repl-mode-map))
  (keymap-set map "C-M-q" #'cider-format-edn-last-sexp)
  (keymap-set map "<remap> <init-describe-symbol-dwim>" #'cider-doc)
  (keymap-set map "<remap> <init-history-placeholder>" #'init-counsel-cider-repl-history))

(defun init-cider-repl-set-xref ()
  "Set Xref backend for Cider REPL."
  (add-hook 'xref-backend-functions #'cider--xref-backend nil t))

(add-hook 'cider-repl-mode-hook #'init-cider-repl-set-xref)

;;;;; macrostep

(defun init-cider-macrostep-macro-form-p (_sexp _env)
  "Macro?"
  t)

(defun init-cider-macrostep-sexp-bounds ()
  "Find bounds of macro sexp."
  (interactive)
  (bounds-of-thing-at-point 'sexp))

(defun init-cider-macrostep-expand (sexp _env)
  "Expand SEXP using Cider."
  (or (cider-sync-request:macroexpand "macroexpand" sexp)
      (user-error "Macro expansion failed")))

(defun init-cider-macrostep-expand-1 (sexp _env)
  "Expand SEXP using Cider."
  (or (cider-sync-request:macroexpand "macroexpand-1" sexp)
      (user-error "Macro expansion failed")))

(defun init-cider-macrostep-insert (sexp _env)
  "Insert expanded SEXP."
  (insert (propertize sexp 'face 'macrostep-expansion-highlight-face)))

(defun init-cider-set-macrostep ()
  "Set Cider macroexpand backends."
  (setq-local macrostep-environment-at-point-function #'ignore)
  (setq-local macrostep-macro-form-p-function #'init-cider-macrostep-macro-form-p)
  (setq-local macrostep-sexp-bounds-function #'init-cider-macrostep-sexp-bounds)
  (setq-local macrostep-sexp-at-point-function #'buffer-substring-no-properties)
  (setq-local macrostep-expand-function #'init-cider-macrostep-expand)
  (setq-local macrostep-expand-1-function #'init-cider-macrostep-expand-1)
  (setq-local macrostep-print-function #'init-cider-macrostep-insert))

(dolist (hook '(cider-mode-hook cider-repl-mode-hook))
  (add-hook hook #'init-cider-set-macrostep))

(dolist (map (list cider-mode-map cider-repl-mode-map))
  (keymap-set map "C-c e" #'macrostep-expand))

;;; python

(require 'python)

(setq python-shell-interpreter "python")
(setq python-shell-interpreter-args "-m IPython --simple-prompt")

(keymap-set python-base-mode-map "C-c C-k" #'python-shell-send-buffer)

(add-to-list 'vim-eval-function-alist '(python-mode . python-shell-send-region))

;;; org

(require 'org)
(require 'org-macs)
(require 'org-agenda)
(require 'org-capture)

(add-to-list 'org-modules 'org-id)
(add-to-list 'org-modules 'org-mouse)
(add-to-list 'org-modules 'org-tempo)
(add-to-list 'org-modules 'ol-eshell)

(setq org-special-ctrl-a/e t)
(setq org-sort-function #'org-sort-function-fallback)
(setq org-tags-sort-function #'org-string<)
(setq org-link-descriptive nil)

(setq org-directory (expand-file-name "org" user-emacs-directory))
(setq org-agenda-files (list org-directory))
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

(setq org-capture-templates
      '(("t" "Todo"                      entry (file "") "* TODO %?\n%U")
        ("a" "Todo With Annotation"      entry (file "") "* TODO %?\n%U\n%a")
        ("i" "Todo With Initial Content" entry (file "") "* TODO %?\n%U\n%i")
        ("c" "Todo With Kill Ring Head"  entry (file "") "* TODO %?\n%U\n%c")))

(defun init-org-set-syntax ()
  "Modify `org-mode' syntax table."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(add-hook 'org-mode-hook #'init-org-set-syntax)

(keymap-global-set "C-c o" #'org-open-at-point-global)
(keymap-global-set "C-c l" #'org-insert-link-global)

(keymap-set org-mode-map "<remap> <org-open-at-point-global>" #'org-open-at-point)
(keymap-set org-mode-map "<remap> <org-insert-link-global>" #'org-insert-link)

(keymap-set org-src-mode-map "C-c C-c" #'org-edit-src-exit)

;;;; roam

(require 'org-roam)

(setq org-roam-directory (expand-file-name "notes" priv-directory))

(setq org-roam-node-display-template
      (concat "${title:*} " (propertize "${tags:30}" 'face 'org-tag)))

(add-hook 'after-init-hook #'org-roam-db-autosync-mode)

(defvar-keymap init-org-roam-command-map
  "n" #'org-roam-node-find
  "l" #'org-roam-node-insert
  "c" #'org-roam-capture
  "b" #'org-roam-buffer-toggle
  "t" #'org-roam-tag-add
  "T" #'org-roam-tag-remove
  "a" #'org-roam-alias-add
  "A" #'org-roam-alias-remove
  "r" #'org-roam-ref-add
  "R" #'org-roam-ref-remove)

(keymap-global-set "C-c n" init-org-roam-command-map)

(defun init-org-roam-node-append ()
  "Append Org Roam node link."
  (interactive)
  (save-excursion
    (unless (eolp)
      (forward-char))
    (call-interactively #'org-roam-node-insert)))

(keymap-set vim-normal-mode-map "<remap> <org-roam-node-insert>" #'init-org-roam-node-append)

;;; markdown

(require 'markdown-mode)

(setq markdown-special-ctrl-a/e t)
(setq markdown-fontify-code-blocks-natively t)

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

(setq default-input-method "pyim")

(defvar init-pyim-zirjma-keymaps
  '(("a"    "a"    "a"          )
    ("b"    "b"    "ou"         )
    ("c"    "c"    "iao"        )
    ("d"    "d"    "uang" "iang")
    ("e"    "e"    "e"          )
    ("f"    "f"    "en"         )
    ("g"    "g"    "eng"        )
    ("h"    "h"    "ang"        )
    ("i"    "ch"   "i"          )
    ("j"    "j"    "an"         )
    ("k"    "k"    "ao"         )
    ("l"    "l"    "ai"         )
    ("m"    "m"    "ian"        )
    ("n"    "n"    "in"         )
    ("o"    "o"    "uo"   "o"   )
    ("p"    "p"    "un"         )
    ("q"    "q"    "iu"         )
    ("r"    "r"    "uan"  "er"  )
    ("s"    "s"    "iong" "ong" )
    ("t"    "t"    "ue"   "ve"  )
    ("u"    "sh"   "u"          )
    ("v"    "zh"   "v"    "ui"  )
    ("w"    "w"    "ia"   "ua"  )
    ("x"    "x"    "ie"         )
    ("y"    "y"    "uai"  "ing" )
    ("z"    "z"    "ei"         )
    ("aa"   "a"                 )
    ("ah"   "ang"               )
    ("ai"   "ai"                )
    ("aj"   "an"                )
    ("ak"   "ao"                )
    ("al"   "ai"                )
    ("an"   "an"                )
    ("ao"   "ao"                )
    ("ee"   "e"                 )
    ("ef"   "en"                )
    ("eg"   "eng"               )
    ("ei"   "ei"                )
    ("en"   "en"                )
    ("er"   "er"                )
    ("ez"   "ei"                )
    ("ob"   "ou"                )
    ("oo"   "o"                 )
    ("ou"   "ou"                )))

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

(pyim-scheme-add
 `(zirjma
   :document "zirjma"
   :class shuangpin
   :first-chars "abcdefghijklmnopqrstuvwxyz"
   :rest-chars "abcdefghijklmnopqrstuvwxyz"
   :prefer-triggers nil
   :cregexp-support-p t
   :keymaps ,init-pyim-zirjma-keymaps))

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
  (let ((char (read-char (concat prefix " C-"))))
    (if (= char ?\C-h)
        (describe-keymap (key-binding (kbd prefix)))
      (let ((literal (= char ?\s))
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
 "SPC" #'switch-to-buffer
 "\\" #'init-magic-shift
 "TAB" #'init-magic-shift
 "c" #'init-magic-C-c
 "u" #'init-magic-C-u
 "z" #'repeat
 "y" #'yank-pop
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
 "A" #'org-agenda
 "C" #'org-capture
 "W" #'org-store-link
 "N" #'org-roam-node-find
 "R" #'org-roam-ref-find
 "w" window-prefix-map
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
 "T" #'init-echo-timestamp-dwim
 "e" #'init-eshell-dwim
 "S" #'init-rg-dwim
 "O" #'init-occur-at-point
 "Q" #'init-query-replace-at-point
 "%" #'query-replace-regexp
 "$" #'ispell-word
 "=" #'apheleia-format-buffer
 "+" #'delete-trailing-whitespace
 "." #'xref-find-definitions
 "?" #'xref-find-references
 "," #'xref-go-back
 "i" #'imenu
 "l" #'counsel-outline
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
