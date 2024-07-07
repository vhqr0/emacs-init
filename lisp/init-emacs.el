;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'dash)
(require 'init-core)

;;; utils

(defun init-diminish-minor-mode (mode)
  (setq minor-mode-alist
        (->> minor-mode-alist
             (--remove (eq (car it) mode)))))

(eval-and-compile
  (defun init-file-name-transforms (x)
    `((".*" ,(expand-file-name x user-emacs-directory) t)))
  (defun init-directory-alist (x)
    `((".*" . ,(expand-file-name x user-emacs-directory)))))

;;; essentials

(prefer-coding-system 'utf-8)

(setq! disabled-command-function nil)

(setq! system-time-locale "C")

(global-set-key (kbd "C-SPC") #'toggle-input-method)

(require 'gcmh)

(init-diminish-minor-mode 'gcmh-mode)

(gcmh-mode 1)

(require 'repeat)

(repeat-mode 1)

;;; files

(setq! vc-handled-backends '(Git))
(setq! vc-make-backup-files t)
(setq! version-control t)
(setq! backup-by-copying t)
(setq! delete-old-versions t)

(setq! auto-save-file-name-transforms (init-file-name-transforms "save/"))
(setq! lock-file-name-transforms      (init-file-name-transforms "lock/"))
(setq! backup-directory-alist         (init-directory-alist      "backup/"))

(setq! auto-save-visited-interval 1)

(add-to-list 'minor-mode-alist '(auto-save-visited-mode " AS"))

;; inhibit save files during initial
(add-hook 'after-init-hook #'auto-save-visited-mode)

(setq! recentf-max-saved-items 200)

(require 'recentf)

;; inhibit access recent file list during initial
(add-hook 'after-init-hook #'recentf-mode)

(require 'bm)

(defvar-keymap init-bm-repeat-map
  :repeat t
  "m" #'bm-toggle
  "n" #'bm-next
  "p" #'bm-previous)

;;; ui

(setq! inhibit-startup-screen t)
(setq! initial-scratch-message nil)

(setq! use-dialog-box nil)
(setq! use-file-dialog nil)

(setq! ring-bell-function #'ignore)

(defvar init-disabled-ui-modes
  '(blink-cursor-mode tooltip-mode tool-bar-mode menu-bar-mode scroll-bar-mode))

(defun init-disable-ui ()
  (interactive)
  (dolist (mode init-disabled-ui-modes)
    (when (fboundp mode)
      (funcall mode -1))))

(init-disable-ui)

(defun init-read-theme (prompt)
  (->> (custom-available-themes)
       (-map #'symbol-name)
       (completing-read prompt)))

(defun init-load-theme (theme)
  (interactive
   (list (intern (init-read-theme "Load custom theme: "))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (message "Load custom theme: %s" theme))

(setq! dashboard-items
       '((recents   . 5)
         (bookmarks . 5)
         (projects  . 5)
         (agenda    . 5)))

(require 'dashboard)

(dashboard-setup-startup-hook)

;;; windows

(require 'winner)

(winner-mode 1)

(require 'windmove)

(windmove-default-keybindings)

(setq! tab-bar-tab-hints t)
(setq! tab-bar-select-tab-modifiers '(meta))

(global-set-key (kbd "C-S-T") #'tab-bar-new-tab)
(global-set-key (kbd "C-S-W") #'tab-bar-close-tab)

(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "C-=") #'text-scale-increase)

;;; indent

(setq-default
 indent-tabs-mode nil
 truncate-lines t)

(setq! word-wrap-by-category t)

(setq! global-hl-line-sticky-flag t)

(require 'display-line-numbers)

(defun init-toggle-line-numbers-type ()
  (interactive)
  (setq-local display-line-numbers-type
              (if (eq display-line-numbers-type 'relative)
                  t
                'relative))
  (display-line-numbers-mode 1))

(global-display-line-numbers-mode 1)

(setq! page-break-lines-lighter nil)

(require 'page-break-lines)

(global-page-break-lines-mode 1)

;;; paredit

(require 'rainbow-delimiters)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(setq! sp-ignore-modes-list nil)
(setq! sp-base-key-bindings 'paredit)
(setq! sp-paredit-bindings
       '(("C-M-f"       . sp-forward-sexp)
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

(require 'smartparens)
(require 'smartparens-config)

(sp-with-modes '(minibuffer-mode)
  (sp-local-pair "'" nil :actions nil))

(init-diminish-minor-mode 'smartparens-mode)

(smartparens-global-mode 1)
(show-smartparens-global-mode 1)

;;; completion

(setq! completion-ignore-case t)
(setq! read-buffer-completion-ignore-case t)
(setq! read-file-name-completion-ignore-case t)

(setq! isearch-lazy-count t)
(setq! isearch-allow-scroll t)
(setq! isearch-allow-motion t)
(setq! isearch-yank-on-move t)
(setq! isearch-motion-changes-direction t)
(setq! isearch-repeat-on-direction-change t)

(setq! hippie-expand-try-functions-list
       '(try-complete-file-name-partially
         try-complete-file-name
         try-expand-dabbrev
         try-expand-dabbrev-all-buffers
         try-expand-dabbrev-from-kill))

(require 'hippie-exp)

(global-set-key (kbd "M-/") #'hippie-expand)

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
  (unless (memq this-command init-evil-adjust-cursor-disabled-commands)
    (apply func args)))

(advice-add #'evil-set-cursor :override #'ignore)
(advice-add #'evil-adjust-cursor :around #'init-around-evil-adjust-cursor)

(define-key evil-insert-state-map (kbd "C-@") nil)
(define-key evil-insert-state-map (kbd "C-a") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-insert-state-map (kbd "C-w") nil)
(define-key evil-normal-state-map [remap yank-pop] nil)

(setq! evil-collection-setup-minibuffer t)

(require 'evil-collection)

(evil-collection-init)

(init-diminish-minor-mode 'evil-collection-unimpaired-mode)

(require 'evil-surround)

(global-evil-surround-mode 1)

(setq! evil-snipe-repeat-keys nil)

(require 'evil-snipe)

(init-diminish-minor-mode 'evil-snipe-local-mode)

(evil-snipe-mode 1)
(evil-snipe-override-mode 1)

(require 'evil-multiedit)

(evil-multiedit-default-keybinds)

(require 'evil-x)

(evil-x-default-keybindings)

;;; company

(setq! company-idle-delay 0.2)
(setq! company-minimum-prefix-length 2)
(setq! company-selection-wrap-around t)
(setq! company-show-quick-access t)
(setq! company-tooltip-width-grow-only t)
(setq! company-tooltip-align-annotations t)
(setq! company-dabbrev-downcase nil)
(setq! company-dabbrev-ignore-case t)
(setq! company-dabbrev-code-ignore-case t)

(setq! company-frontends
       '(company-pseudo-tooltip-frontend
         company-preview-if-just-one-frontend
         company-echo-metadata-frontend))

(setq! company-backends
       '(company-files
         (company-capf :with company-yasnippet)
         (company-dabbrev-code company-keywords :with company-yasnippet)
         (company-dabbrev company-yasnippet)))

(require 'company)
(require 'company-capf)

(global-company-mode 1)

(defvar-keymap init-company-prefix-map
  "<tab>" #'company-complete
  "TAB"   #'company-complete
  "c"     #'company-capf
  "f"     #'company-files
  "/"     #'company-dabbrev
  "y"     #'company-yasnippet)

(global-set-key (kbd "C-c c") init-company-prefix-map)

;;; yasnippet

(setq! yas-alias-to-yas/prefix-p nil)

(require 'yasnippet)

(init-diminish-minor-mode 'yas-minor-mode)

(yas-global-mode 1)

(defvar-keymap init-yasnippet-prefix-map
  "<tab>" #'yas-expand-from-trigger-key
  "TAB"   #'yas-expand-from-trigger-key
  "s"     #'yas-insert-snippet
  "n"     #'yas-new-snippet
  "v"     #'yas-visit-snippet-file
  "w"     #'aya-create
  "y"     #'aya-expand
  "Y"     #'aya-expand-from-history)

(global-set-key (kbd "C-c y") init-yasnippet-prefix-map)

;;; flymake

(require 'flymake)

(define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error)

;;; helm

(setq! helm-echo-input-in-header-line t)
(setq! helm-move-to-line-cycle-in-source nil)
(setq! helm-window-prefer-horizontal-split t)
(setq! helm-completion-style 'helm-fuzzy)
(setq! helm-buffers-fuzzy-matching t)
(setq! helm-recentf-fuzzy-match t)
(setq! helm-file-cache-fuzzy-match t)
(setq! helm-locate-fuzzy-match t)
(setq! helm-ls-git-fuzzy-match t)
(setq! helm-etags-fuzzy-match t)
(setq! helm-apropos-fuzzy-match t)
(setq! helm-session-fuzzy-match t)
(setq! helm-bookmark-show-location t)
(setq! helm-buffer-max-length 40)
(setq! helm-buffer-skip-remote-checking t)
(setq! helm-grep-file-path-style 'relative)

(require 'helm-mode)

(advice-add #'helm-minibuffer-history-mode :override #'ignore)

(add-to-list 'helm-completing-read-handlers-alist '(kill-buffer . nil))

(init-diminish-minor-mode 'helm-mode)

(helm-mode 1)

(require 'helm-x)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "M-y") #'helm-show-kill-ring)
(global-set-key (kbd "C-c b") #'helm-resume)
(global-set-key (kbd "C-c h") #'helm-x-history)
(global-set-key (kbd "C-c i") #'helm-x-imenu)
(global-set-key (kbd "C-c I") #'helm-x-imenu-all)

(define-key helm-occur-mode-map [remap helm-occur-mode-goto-line] #'helm-occur-mode-goto-line-ow)
(define-key helm-grep-mode-map [remap helm-grep-mode-jump] #'helm-grep-mode-jump-other-window)

(evil-collection-define-key 'normal 'helm-map
  (kbd "SPC") nil
  "m" 'helm-toggle-visible-mark
  "U" 'helm-unmark-all)

(evil-collection-define-key '(insert normal) 'helm-map
  (kbd "C-SPC") 'toggle-input-method
  (kbd "C-t") 'helm-toggle-resplit-and-swap-windows)

;;; project

(setq! projectile-keymap-prefix (kbd "C-x p"))
(setq! helm-projectile-truncate-lines t)
(setq! projectile-current-project-on-switch 'move-to-end)
(setq! projectile-switch-project-action #'helm-projectile-find-file)

(require 'projectile)
(require 'helm-projectile)

(projectile-mode 1)
(helm-projectile-on)

(advice-add 'helm-projectile-rg :override #'projectile-ripgrep)

;;; git

(setq! forge-add-default-bindings nil)

(require 'magit)
(require 'forge)

;;; dired

(setq! dired-dwim-target t)
(setq! dired-listing-switches "-lha")

(require 'dired)

(put 'dired-jump 'repeat-map nil)

;;; ibuffer

(setq! ibuffer-formats
       '((mark " " modified read-only locked
               " " (name 40 40 :left :elide)
               " " (size 9 -1 :right)
               " " (mode 16 16 :left :elide)
               " " filename-and-process)))

;;; grep

(setq! wgrep-auto-save-buffer t)
(setq! wgrep-change-readonly-file t)

(autoload 'rg-menu "rg" nil t)
(declare-function rg-menu "rg")

;;; eshell

(defvar eshell-mode-map)

(defun init-eshell-set-company ()
  (setq-local company-backends '(company-files)))

(defun init-eshell-remap-pcomplete ()
  (define-key eshell-mode-map [remap completion-at-point] #'helm-esh-pcomplete))

(add-hook 'eshell-mode-hook #'with-editor-export-editor)
(add-hook 'eshell-mode-hook #'init-eshell-set-company)
(add-hook 'eshell-mode-hook #'init-eshell-remap-pcomplete)

(declare-function evil-collection-eshell-escape-stay "evil-collection-eshell")
(advice-add #'evil-collection-eshell-escape-stay :override #'ignore)

(autoload 'eshell-dwim "eshell-x" nil t)
(autoload 'project-eshell-dwim "eshell-x" nil t)
(declare-function eshell-dwim "eshell-x")
(declare-function projecit-eshell-dwim "eshell-x")

;;; help

(require 'find-func)

(find-function-setup-keys)

(setq! helpful-max-buffers nil)

(require 'helpful)

(setq! helm-describe-function-function #'helpful-callable)
(setq! helm-describe-variable-function #'helpful-variable)

(defun init-lookup-setup-command (command)
  (setq-local evil-lookup-func command)
  (local-set-key [remap display-local-help] command))

(defun init-lookup-setup-helpful () (init-lookup-setup-command #'helpful-at-point))
(defun init-lookup-setup-woman   () (init-lookup-setup-command #'woman))

(defvar init-lookup-helpful-mode-hooks
  '(emacs-lisp-mode-hook lisp-interaction-mode-hook help-mode-hook helpful-mode-hook Info-mode-hook))

(defvar init-lookup-woman-mode-hooks
  '(c-mode-common-hook sh-mode-hook shell-mode-hook eshell-mode-hook man-mode-hook woman-mode-hook))

(dolist (hook init-lookup-helpful-mode-hooks)
  (add-hook hook #'init-lookup-setup-helpful))

(dolist (hook init-lookup-woman-mode-hooks)
  (add-hook hook #'init-lookup-setup-woman))

;;; context

(require 'which-key)

(init-diminish-minor-mode 'which-key-mode)

(which-key-mode 1)

(require 'embark)

(global-set-key (kbd "M-o") #'embark-act)

(require 'embark-x)

(embark-which-key-enable)

(defun init-open-files (&optional files)
  (interactive)
  (require 'helm-utils)
  (let* ((files (cond (files)
                      (buffer-file-name)
                      ((eq major-mode 'dired-mode)
                       (dired-get-marked-files))
                      (default-directory)))
         (files (if (listp files) files (list files))))
    (dolist (file files)
      (helm-open-file-with-default-tool file))))

;;; lisp

(setq! evil-cleverparens-use-s-and-S nil)
(setq! evil-cleverparens-use-regular-insert t)
(setq! evil-cleverparens-use-additional-bindings nil)
(setq! evil-cleverparens-use-additional-movement-keys nil)

(require 'evil-cleverparens)

(init-diminish-minor-mode 'evil-cleverparens-mode)

(defun init-enable-smartparens ()
  (interactive)
  (smartparens-strict-mode 1)
  (evil-cleverparens-mode 1))

(defvar init-lisp-mode-hooks
  '(lisp-data-mode-hook emacs-lisp-mode-hook lisp-interaction-mode-hook lisp-mode-hook))

(dolist (hook init-lisp-mode-hooks)
  (add-hook hook #'init-enable-smartparens))

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (define-key map (kbd "C-c e") #'macrostep-expand))

;;; markdown

(setq! markdown-fontify-code-blocks-natively t)

;;; org

(setq! org-directory (expand-file-name "org" user-emacs-directory))
(setq! org-agenda-files (list org-directory))
(setq! org-default-notes-file (expand-file-name "inbox.org" org-directory))

(setq! org-capture-templates
       '(("t" "Task" entry (file+headline "" "Tasks") "* TODO %?\n%U\n%a")))

(require 'org)

(add-to-list 'org-modules 'org-tempo)

(define-key org-mode-map (kbd "C-c l") #'org-toggle-link-display)

(defun init-org-modify-syntax ()
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(add-hook 'org-mode-hook #'init-org-modify-syntax)

(setq! evil-org-key-theme
       '(navigation return textobjects additional calendar))

(require 'evil-org)
(require 'evil-org-agenda)

(init-diminish-minor-mode 'evil-org-mode)

(evil-org-agenda-set-keys)

;;; leader maps

(defvar init-leader-map)

(defun init-magic-universal-argument ()
  (interactive)
  (setq prefix-arg
        (list (if current-prefix-arg
                  (* 4 (prefix-numeric-value current-prefix-arg))
                4)))
  (set-transient-map init-leader-map))

(autoload 'god-mode-self-insert "god-mode" nil t)
(declare-function god-mode-self-insert "god-mode")

(defvar-keymap init-leader-map
  "SPC" #'helm-mini
  "u" #'init-magic-universal-argument
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
  "w w" #'evil-window-next
  "w W" #'evil-window-prev
  "w q" #'evil-quit
  "w c" #'evil-window-delete
  "w 0" #'evil-window-delete
  "w o" #'delete-other-windows
  "w s" #'evil-window-split
  "w v" #'evil-window-vsplit
  "w =" #'balance-windows
  "w x" #'evil-window-exchange
  "w j" #'evil-window-down
  "w k" #'evil-window-up
  "w h" #'evil-window-left
  "w l" #'evil-window-right
  "w J" #'evil-window-move-very-bottom
  "w K" #'evil-window-move-very-top
  "w H" #'evil-window-move-far-left
  "w L" #'evil-window-move-far-right
  "w <left>" #'winner-undo
  "w <right>" #'winner-redo
  "f" #'helm-find-files
  "b" #'helm-buffers-list
  "j" #'dired-jump
  "k" #'kill-buffer
  "4 f" #'find-file-other-window
  "4 b" #'switch-to-buffer-other-window
  "4 j" #'dired-jump-other-window
  "5 0" #'delete-frame
  "5 1" #'delete-other-frames
  "5 2" #'make-frame-command
  "5 o" #'other-frame
  "5 u" #'undelete-frame
  "t 0" #'tab-bar-close-tab
  "t 1" #'tab-bar-close-group-tabs
  "t 2" #'tab-bar-new-tab
  "t o" #'tab-bar-switch-to-next-tab
  "t O" #'tab-bar-switch-to-prev-tab
  "t u" #'tab-bar-undo-close-tab
  "r m" #'bookmark-set
  "r b" #'helm-bookmarks
  "r e" #'helm-recentf
  "r w" #'org-store-link
  "r a" #'org-agenda
  "r c" #'org-capture
  "x g" #'revert-buffer-quick
  "x G" #'revert-buffer
  "x v" #'vc-refresh-state
  "x f" #'font-lock-update
  "x o" #'init-open-files
  "x m" #'bm-toggle
  "x n" #'bm-next
  "x p" #'bm-previous
  "x M" #'bm-remove-all-current-buffer
  "x <left>" #'previous-buffer
  "x <right>" #'next-buffer
  "p p" #'projectile-switch-project
  "p i" #'projectile-invalidate-cache
  "p f" #'projectile-find-file
  "p b" #'projectile-switch-to-buffer
  "p j" #'projectile-dired
  "4 p f" #'projectile-find-file-other-window
  "4 p b" #'projectile-switch-to-buffer-other-window
  "4 p j" #'projectile-dired-other-window
  "p s" #'projectile-save-project-buffers
  "p k" #'projectile-kill-buffers
  "p x" #'projectile-run-command-in-root
  "p c" #'projectile-compile-project
  "p !" #'projectile-run-shell-command-in-root
  "p &" #'projectile-run-async-shell-command-in-root
  "p v" #'projectile-vc
  "p g" #'projectile-ripgrep
  "v v" #'magit-status
  "v V" #'magit-dispatch
  "v ?" #'magit-file-dispatch
  "v g" #'magit-status-here
  "v G" #'magit-display-repository-buffer
  "v s" #'magit-stage-buffer-file
  "v u" #'magit-unstage-buffer-file
  "v d" #'magit-diff-buffer-file
  "v D" #'magit-diff
  "v l" #'magit-log-buffer-file
  "v L" #'magit-log
  "v b" #'magit-blame-addition
  "v B" #'magit-blame
  "v f" #'magit-find-file
  "v F" #'magit-blob-visit-file
  "v n" #'magit-blob-next
  "v p" #'magit-blob-previous
  "v t" #'git-timemachine
  "l" #'ibuffer
  "p l" #'projectile-ibuffer
  "e" #'eshell-dwim
  "p e" #'project-eshell-dwim
  "n w" #'widen
  "n n" #'narrow-to-region
  "n d" #'narrow-to-defun
  "n p" #'narrow-to-page
  "g g" #'rg-menu
  "g d" #'rg-dwim
  "g c" #'rg-dwim-current-dir
  "g f" #'rg-dwim-current-file
  "g o" #'occur
  "g n" #'next-error
  "g p" #'previous-error
  "s" #'helm-occur
  "S" #'helm-x-grep
  "F" #'helm-x-find
  "i" #'helm-x-imenu
  "I" #'helm-x-imenu-all
  "$" #'ispell-word
  "%" #'query-replace-regexp
  "=" #'format-all-region-or-buffer
  "." #'xref-find-definitions
  "?" #'xref-find-references
  "," #'xref-go-back
  "4 ." #'xref-find-definitions-other-window
  "(" #'sp-wrap-round
  "[" #'sp-wrap-square
  "{" #'sp-wrap-curly
  "m a" #'auto-save-visited-mode
  "m t" #'toggle-truncate-lines
  "m h" #'hl-line-mode
  "m l" #'display-line-numbers-mode
  "m L" #'init-toggle-line-numbers-type
  "m s" #'whitespace-mode
  "m v" #'visual-line-mode
  "h h" #'help-for-help
  "h ." #'display-local-help
  "h i" #'info
  "h l" #'view-lossage
  "h e" #'view-echo-area-messages
  "h d" #'dashboard-open
  "h s" #'scratch-buffer
  "h o" #'helm-apropos
  "h x" #'helpful-command
  "h f" #'helpful-function
  "h v" #'helpful-variable
  "h p" #'describe-package
  "h m" #'describe-mode
  "h b" #'describe-bindings
  "h B" #'describe-keymap
  "h w" #'where-is
  "h k" #'helpful-key
  "h c" #'describe-key-briefly
  "h t l" #'load-library
  "h t f" #'load-file
  "h t t" #'init-load-theme
  "h L" #'find-library
  "h F" #'find-function
  "h V" #'find-variable
  "h K" #'find-function-on-key
  "4 h i" #'info-other-window
  "4 h L" #'find-library-other-window
  "4 h F" #'find-function-other-window
  "4 h V" #'find-variable-other-window
  "4 h K" #'find-function-on-key-other-window)

(defvar init-leader-override-mode-map (make-sparse-keymap))

(define-minor-mode init-leader-override-mode
  "Override leader prefix map."
  :group 'init
  :global t
  :keymap init-leader-override-mode-map)

(init-leader-override-mode 1)

(evil-define-key '(motion normal visual operator) init-leader-override-mode-map
  (kbd "SPC") init-leader-map)

(evil-define-key '(insert) init-leader-override-mode-map
  (kbd "M-r")  #'helm-x-history)

;;; end

(provide 'init-emacs)
