;;; init-emacs.el --- Init Emacs -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Emacs itself.

(require 'cl-lib)
(require 'dash)
(require 'init-core)

;;; Code:

;;; utils

(defun init-diminish-minor-mode (mode)
  "Diminish MODE lighter."
  (setq minor-mode-alist
        (->> minor-mode-alist
             (--remove (eq (car it) mode)))))

;;; essentials

(prefer-coding-system 'utf-8)

(setq! disabled-command-function nil)

(setq! system-time-locale "C")

(setq! read-process-output-max (* 1024 1024))

(require 'gcmh)

(init-diminish-minor-mode 'gcmh-mode)

(gcmh-mode 1)

(require 'repeat)

(repeat-mode 1)

(global-set-key (kbd "M-o") #'embark-act)

(global-set-key (kbd "C-SPC") #'toggle-input-method)

;;; files

(setq! vc-handled-backends '(Git))
(setq! vc-make-backup-files t)
(setq! version-control t)
(setq! backup-by-copying t)
(setq! delete-old-versions t)

(eval-and-compile
  (defun init-file-name-transforms (x)
    `((".*" ,(expand-file-name x user-emacs-directory) t)))
  (defun init-directory-alist (x)
    `((".*" . ,(expand-file-name x user-emacs-directory)))))

(setq! auto-save-file-name-transforms (init-file-name-transforms "save/"))
(setq! lock-file-name-transforms      (init-file-name-transforms "lock/"))
(setq! backup-directory-alist         (init-directory-alist      "backup/"))

(setq! auto-save-visited-interval 1)

(add-to-list 'minor-mode-alist '(auto-save-visited-mode " AS"))

(auto-save-visited-mode 1)

(setq! recentf-max-saved-items 200)

(require 'recentf)

(recentf-mode 1)

;;; ui

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

;;; lines

(setq-default
 indent-tabs-mode nil
 truncate-lines t)

(setq! word-wrap-by-category t)

(setq! global-hl-line-sticky-flag t)

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

(defun init-set-trailing-whitespace-display ()
  "Set local display of trailing whitespace."
  (setq-local show-trailing-whitespace t))

(add-hook 'text-mode-hook #'init-set-trailing-whitespace-display)
(add-hook 'prog-mode-hook #'init-set-trailing-whitespace-display)

(require 'ws-butler)

(init-diminish-minor-mode 'ws-butler-mode)

(add-hook 'text-mode-hook #'ws-butler-mode)
(add-hook 'prog-mode-hook #'ws-butler-mode)

(setq! page-break-lines-lighter nil)

(require 'page-break-lines)

(global-page-break-lines-mode 1)

(require 'bm)

(defvar-keymap init-bm-repeat-map
  :repeat t
  "m" #'bm-toggle
  "n" #'bm-next
  "p" #'bm-previous)

;;; parens

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
  "Dont adjust cursor after certain commands.
FUNC and ARGS see `evil-set-cursor'."
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
  (let ((eval-function (cdr (assq major-mode init-evil-eval-function-alist))))
    (when eval-function
      (funcall eval-function beg end))))

(evil-define-operator init-evil-operator-format (beg end)
  :move-point nil
  (interactive "<r>")
  (format-all-region beg end))

(evil-define-text-object init-evil-inner-line (count &optional _beg _end _type)
  (evil-range
   (save-excursion (goto-char (line-beginning-position)) (back-to-indentation) (point))
   (line-end-position)
   'exclusive))

(evil-define-text-object init-evil-a-line (count &optional _beg _end _type)
  (evil-range (line-beginning-position) (line-end-position) 'line))

(evil-define-text-object init-evil-inner-defun (count &optional beg end _type)
  (evil-select-inner-object 'evil-defun beg end type count t))

(evil-define-text-object init-evil-a-defun (count &optional beg end _type)
  (evil-select-an-object 'evil-defun beg end type count t))

(evil-define-text-object init-evil-text-object-url (count &optional _beg _end _type)
  (cl-destructuring-bind (beg . end) (bounds-of-thing-at-point 'filename)
    (evil-range beg end 'inclusive)))

(evil-define-text-object init-evil-text-object-entire (count &optional _beg _end _type)
  (evil-range (point-min) (point-max) 'line))

(evil-define-text-object init-evil-inner-comment (count &optional _beg _end _type)
  (cl-destructuring-bind (beg . end) (sp-get-comment-bounds)
    (evil-range
     (save-excursion (goto-char beg) (forward-word 1) (forward-word -1) (point))
     (save-excursion (goto-char end) (evil-end-of-line) (point))
     'block)))

(evil-define-text-object init-evil-a-comment (count &optional _beg _end _type)
  (cl-destructuring-bind (beg . end) (sp-get-comment-bounds)
    (evil-range beg end 'exclusive)))

(define-key evil-insert-state-map "j" #'init-evil-escape)
(define-key evil-replace-state-map "j" #'init-evil-escape)
(define-key evil-motion-state-map "%" #'init-evil-jump-item)
(define-key evil-normal-state-map "gc" #'init-evil-operator-comment)
(define-key evil-motion-state-map "g-" #'init-evil-operator-narrow)
(define-key evil-motion-state-map "gy" #'init-evil-operator-eval)
(define-key evil-motion-state-map "g=" #'init-evil-operator-format)
(define-key evil-inner-text-objects-map "l" #'init-evil-inner-line)
(define-key evil-outer-text-objects-map "l" #'init-evil-a-line)
(define-key evil-inner-text-objects-map "d" #'init-evil-inner-defun)
(define-key evil-outer-text-objects-map "d" #'init-evil-a-defun)
(define-key evil-inner-text-objects-map "c" #'init-evil-inner-comment)
(define-key evil-outer-text-objects-map "c" #'init-evil-a-comment)
(define-key evil-inner-text-objects-map "u" #'init-evil-text-object-url)
(define-key evil-outer-text-objects-map "u" #'init-evil-text-object-url)
(define-key evil-inner-text-objects-map "h" #'init-evil-text-object-entire)
(define-key evil-outer-text-objects-map "h" #'init-evil-text-object-entire)

(defvar init-evil-override-mode-map (make-sparse-keymap))

(define-minor-mode init-evil-override-mode
  "Override leader prefix map."
  :group 'init
  :global t
  :keymap init-evil-override-mode-map)

(init-evil-override-mode 1)

;;; helm

(setq! helm-echo-input-in-header-line t)
(setq! helm-move-to-line-cycle-in-source nil)
(setq! helm-window-prefer-horizontal-split t)
(setq! helm-completion-style 'helm-fuzzy)
(setq! helm-buffers-fuzzy-matching t)
(setq! helm-recentf-fuzzy-match t)
(setq! helm-bookmark-show-location t)
(setq! helm-file-cache-fuzzy-match t)
(setq! helm-locate-fuzzy-match t)
(setq! helm-ls-git-fuzzy-match t)
(setq! helm-imenu-fuzzy-match t)
(setq! helm-etags-fuzzy-match t)
(setq! helm-apropos-fuzzy-match t)
(setq! helm-session-fuzzy-match t)
(setq! helm-buffer-max-length 40)
(setq! helm-grep-file-path-style 'relative)
(setq! helm-grep-save-buffer-name-no-confirm t)

(require 'helm-mode)
(require 'helm-descbinds)

(add-to-list 'helm-completing-read-handlers-alist '(kill-buffer . nil))

(advice-add #'helm-minibuffer-history-mode :override #'ignore)

(init-diminish-minor-mode 'helm-mode)

(helm-mode 1)

(helm-descbinds-mode 1)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "M-y") #'helm-show-kill-ring)
(global-set-key (kbd "C-c b") #'helm-resume)

(defun init-history-placeholder ()
  "Placeholder for `M-r' to search history in insert mode."
  (interactive))

(evil-define-key '(insert) init-evil-override-mode-map
  (kbd "M-r")  #'init-history-placeholder)

(define-key minibuffer-mode-map [remap init-history-placeholder] #'helm-minibuffer-history)

(defun init-open-files (&optional files)
  "Open FILES externally with `helm-open-file-with-default-tool'."
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

(define-key helm-buffer-map [remap helm-occur] #'helm-buffers-run-occur)
(define-key helm-generic-files-map [remap helm-occur] #'helm-ff-run-grep)
(define-key helm-generic-files-map [remap init-open-files] #'helm-ff-run-open-file-with-default-tool)

(define-key helm-find-files-map [remap helm-do-grep-ag] #'helm-ff-run-grep-ag)
(define-key helm-find-files-map [remap helm-find] #'helm-ff-run-find-sh-command)
(define-key helm-find-files-map [remap helm-occur] #'helm-ff-run-grep)
(define-key helm-find-files-map [remap init-open-files] #'helm-ff-run-open-file-with-default-tool)

(define-key helm-occur-mode-map [remap helm-occur-mode-goto-line] #'helm-occur-mode-goto-line-ow)
(define-key helm-grep-mode-map [remap helm-grep-mode-jump] #'helm-grep-mode-jump-other-window)

(evil-collection-define-key 'normal 'helm-map
  (kbd "SPC") nil
  "m" 'helm-toggle-visible-mark
  "U" 'helm-unmark-all)

(evil-collection-define-key '(insert normal) 'helm-map
  (kbd "C-SPC") 'toggle-input-method
  (kbd "C-t") 'helm-toggle-resplit-and-swap-windows)

;;; isearch

(setq! isearch-lazy-count t)
(setq! isearch-allow-scroll t)
(setq! isearch-allow-motion t)
(setq! isearch-yank-on-move t)
(setq! isearch-motion-changes-direction t)
(setq! isearch-repeat-on-direction-change t)

(require 'swiper)

(global-set-key (kbd "C-s") #'swiper-isearch)
(global-set-key (kbd "C-r") #'swiper-isearch-backward)

(define-key swiper-isearch-map [remap init-history-placeholder] #'swiper-isearch-C-r)
(define-key swiper-isearch-map (kbd "TAB") #'swiper-isearch-toggle)
(define-key isearch-mode-map (kbd "TAB") #'swiper-isearch-toggle)

(define-key evil-motion-state-map "/" #'swiper-isearch)
(define-key evil-motion-state-map "?" #'swiper-isearch-backward)

(define-key evil-operator-state-map "/" #'evil-search-forward)
(define-key evil-operator-state-map "?" #'evil-search-backward)

(defun init-after-swiper-isearch-forward (&rest _)
  "Reset `v/isearch-forward' after `swiper'."
  (setq isearch-forward t isearch-regexp t))

(defun init-after-swiper-isearch-backward (&rest _)
  "Reset `v/isearch-forward' after `swiper'."
  (setq isearch-forward nil isearch-regexp t))

(advice-add #'swiper-isearch :after #'init-after-swiper-isearch-forward)
(advice-add #'swiper-isearch-backward :after #'init-after-swiper-isearch-backward)

;;; help

(require 'find-func)

(find-function-setup-keys)

(setq! helpful-max-buffers nil)

(require 'helpful)

(setq! helm-describe-function-function #'helpful-callable)
(setq! helm-describe-variable-function #'helpful-variable)

(setq! helm-man-or-woman-function #'woman)

(defun init-lookup-setup-command (command)
  "Setup COMMAND as local help command."
  (setq-local evil-lookup-func command)
  (local-set-key [remap display-local-help] command))

(defun init-lookup-setup-helpful () "Setup helpful." (init-lookup-setup-command #'helpful-at-point))
(defun init-lookup-setup-woman   () "Setup woman."   (init-lookup-setup-command #'helm-man-woman))

(defvar init-lookup-helpful-mode-hooks
  '(emacs-lisp-mode-hook lisp-interaction-mode-hook help-mode-hook helpful-mode-hook Info-mode-hook))

(defvar init-lookup-woman-mode-hooks
  '(c-mode-common-hook sh-mode-hook shell-mode-hook eshell-mode-hook man-mode-hook woman-mode-hook))

(dolist (hook init-lookup-helpful-mode-hooks)
  (add-hook hook #'init-lookup-setup-helpful))

(dolist (hook init-lookup-woman-mode-hooks)
  (add-hook hook #'init-lookup-setup-woman))

;;; project

(setq! projectile-keymap-prefix (kbd "C-x p"))
(setq! projectile-current-project-on-switch 'move-to-end)
(setq! projectile-switch-project-action #'helm-projectile-find-file)
(setq! helm-projectile-truncate-lines t)

(require 'projectile)
(require 'helm-projectile)

(projectile-mode 1)
(helm-projectile-on)

(advice-add #'helm-projectile-rg :override #'projectile-ripgrep)

;;; prog

;;;; company

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

(global-set-key (kbd "C-c c") #'company-complete)

;;;; yasnippet

(setq! yas-alias-to-yas/prefix-p nil)

(require 'yasnippet)

(init-diminish-minor-mode 'yas-minor-mode)

(yas-global-mode 1)

(global-set-key (kbd "C-c y") #'yas-expand-from-trigger-key)

;;;; flycheck

(require 'flycheck)

(define-key flycheck-mode-map (kbd "M-n") #'flycheck-next-error)
(define-key flycheck-mode-map (kbd "M-p") #'flycheck-previous-error)

;;; tools

;;;; dired

(setq! dired-dwim-target t)
(setq! dired-auto-revert-buffer t)
(setq! dired-kill-when-opening-new-dired-buffer t)
(setq! dired-listing-switches "-lha")

(require 'dired)
(require 'dired-x)
(require 'diredfl)

(put 'dired-jump 'repeat-map nil)

(add-to-list 'dired-mode-hook #'diredfl-mode)

(define-key dired-mode-map "O" #'dired-omit-mode)

(evil-collection-define-key 'normal 'dired-mode-map
  "O" #'dired-omit-mode)

;;;; git

(autoload 'magit-blob-next "magit" nil t)
(autoload 'magit-blob-previous "magit" nil t)

(require 'diff-hl)

(global-diff-hl-mode 1)

;;;; grep

(setq! wgrep-auto-save-buffer t)
(setq! wgrep-change-readonly-file t)

(autoload 'rg-menu "rg" nil t)
(declare-function rg-menu "rg")

;;;; shell

(require 'comint)

(define-key comint-mode-map [remap helm-imenu] #'helm-comint-prompts)
(define-key comint-mode-map [remap helm-imenu-in-all-buffers] #'helm-comint-prompts-all)
(define-key comint-mode-map [remap init-history-placeholder] #'helm-comint-input-ring)

;;;; eshell

(defvar eshell-mode-map)

(defun init-eshell-set-company ()
  "Clean company backends."
  (setq-local company-backends '(company-files)))

(defun init-eshell-remap-helm ()
  "Remap eshell local maps to helm commands."
  (define-key eshell-mode-map [remap completion-at-point] #'helm-esh-pcomplete)
  (define-key eshell-mode-map [remap helm-imenu] #'helm-eshell-prompts)
  (define-key eshell-mode-map [remap helm-imenu-in-all-buffers] #'helm-eshell-prompts-all)
  (define-key eshell-mode-map [remap init-history-placeholder] #'helm-eshell-history))

(add-hook 'eshell-mode-hook #'with-editor-export-editor)
(add-hook 'eshell-mode-hook #'init-eshell-set-company)
(add-hook 'eshell-mode-hook #'init-eshell-remap-helm)

(declare-function evil-collection-eshell-escape-stay "evil-collection-eshell")
(advice-add #'evil-collection-eshell-escape-stay :override #'ignore)

;;;; spell

(setq! ispell-dictionary "american")

;;; lisp

(advice-add 'evil-cp--enable-text-objects :override #'ignore)

(setq! evil-cleverparens-use-s-and-S nil)
(setq! evil-cleverparens-use-regular-insert t)
(setq! evil-cleverparens-use-additional-bindings nil)
(setq! evil-cleverparens-use-additional-movement-keys nil)

(require 'evil-cleverparens)

(init-diminish-minor-mode 'evil-cleverparens-mode)

(defun init-enable-smartparens ()
  "Init smartparens related modes for lispy mode."
  (interactive)
  (smartparens-strict-mode 1)
  (evil-cleverparens-mode 1))

(defvar init-lisp-mode-hooks
  '(lisp-data-mode-hook emacs-lisp-mode-hook lisp-interaction-mode-hook lisp-mode-hook))

(dolist (hook init-lisp-mode-hooks)
  (add-hook hook #'init-enable-smartparens))

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (define-key map (kbd "C-c e") #'macrostep-expand))

(require 'flycheck-package)

(setq! flycheck-emacs-lisp-load-path load-path)

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

;;; markdown

(setq! markdown-fontify-code-blocks-natively t)

(require 'markdown-mode)
(require 'edit-indirect)

(define-key markdown-mode-map (kbd "C-c C-'") #'markdown-edit-code-block)
(define-key edit-indirect-mode-map (kbd "C-c C-'") #'edit-indirect-commit)

;;; org

(setq! org-directory (expand-file-name "org" user-emacs-directory))
(setq! org-agenda-files (list org-directory))
(setq! org-default-notes-file (expand-file-name "inbox.org" org-directory))

(setq! org-capture-templates
       '(("t" "Task" entry (file+headline "" "Tasks") "* TODO %?\n%U\n%a")))

(require 'org)

(add-to-list 'org-modules 'org-tempo)

(defun init-org-modify-syntax ()
  "Modify `org-mode' syntax table."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(add-hook 'org-mode-hook #'init-org-modify-syntax)

(define-key org-mode-map (kbd "C-c l") #'org-toggle-link-display)

(define-key org-mode-map (kbd "C-c C-'") #'org-edit-special)
(define-key org-src-mode-map (kbd "C-c C-'") #'org-edit-src-exit)
(define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

(define-key org-mode-map [remap helm-imenu] #'helm-org-in-buffer-headings)
(define-key org-mode-map [remap helm-imenu-in-all-buffers] #'helm-org-agenda-files-headings)

(setq! evil-org-key-theme
       '(navigation return textobjects additional calendar))

(require 'evil-org)
(require 'evil-org-agenda)

(init-diminish-minor-mode 'evil-org-mode)

(evil-org-agenda-set-keys)

;;; leaders

(defvar init-leader-map (make-sparse-keymap))

(evil-define-key '(motion normal visual operator) init-evil-override-mode-map
  (kbd "SPC") init-leader-map)

(defun init-leader-universal-argument ()
  "Magic universal arguments for `init-leader-map'."
  (interactive)
  (setq prefix-arg
        (list (if current-prefix-arg
                  (* 4 (prefix-numeric-value current-prefix-arg))
                4)))
  (set-transient-map init-leader-map))

(require 'god-mode)

(define-key init-leader-map (kbd "SPC") #'helm-mini)
(define-key init-leader-map (kbd "u") #'init-leader-universal-argument)
(define-key init-leader-map (kbd "c") #'god-mode-self-insert)
(define-key init-leader-map (kbd "z") #'repeat)
(define-key init-leader-map (kbd ";") #'eval-expression)
(define-key init-leader-map (kbd "!") #'shell-command)
(define-key init-leader-map (kbd "&") #'async-shell-command)
(define-key init-leader-map (kbd "0") #'delete-window)
(define-key init-leader-map (kbd "1") #'delete-other-windows)
(define-key init-leader-map (kbd "2") #'split-window-below)
(define-key init-leader-map (kbd "3") #'split-window-right)
(define-key init-leader-map (kbd "o") #'other-window)
(define-key init-leader-map (kbd "q") #'quit-window)
(define-key init-leader-map (kbd "`") #'tmm-menubar)

(define-key init-leader-map (kbd "w w") #'evil-window-next)
(define-key init-leader-map (kbd "w W") #'evil-window-prev)
(define-key init-leader-map (kbd "w q") #'evil-quit)
(define-key init-leader-map (kbd "w c") #'evil-window-delete)
(define-key init-leader-map (kbd "w 0") #'evil-window-delete)
(define-key init-leader-map (kbd "w o") #'delete-other-windows)
(define-key init-leader-map (kbd "w s") #'evil-window-split)
(define-key init-leader-map (kbd "w v") #'evil-window-vsplit)
(define-key init-leader-map (kbd "w =") #'balance-windows)
(define-key init-leader-map (kbd "w x") #'evil-window-exchange)
(define-key init-leader-map (kbd "w j") #'evil-window-down)
(define-key init-leader-map (kbd "w k") #'evil-window-up)
(define-key init-leader-map (kbd "w h") #'evil-window-left)
(define-key init-leader-map (kbd "w l") #'evil-window-right)
(define-key init-leader-map (kbd "w J") #'evil-window-move-very-bottom)
(define-key init-leader-map (kbd "w K") #'evil-window-move-very-top)
(define-key init-leader-map (kbd "w H") #'evil-window-move-far-left)
(define-key init-leader-map (kbd "w L") #'evil-window-move-far-right)
(define-key init-leader-map (kbd "w <left>") #'winner-undo)
(define-key init-leader-map (kbd "w <right>") #'winner-redo)

(define-key init-leader-map (kbd "f") #'helm-find-files)
(define-key init-leader-map (kbd "b") #'helm-buffers-list)
(define-key init-leader-map (kbd "j") #'dired-jump)
(define-key init-leader-map (kbd "k") #'kill-buffer)
(define-key init-leader-map (kbd "4 f") #'find-file-other-window)
(define-key init-leader-map (kbd "4 b") #'switch-to-buffer-other-window)
(define-key init-leader-map (kbd "4 j") #'dired-jump-other-window)
(define-key init-leader-map (kbd "5 0") #'delete-frame)
(define-key init-leader-map (kbd "5 1") #'delete-other-frames)
(define-key init-leader-map (kbd "5 2") #'make-frame-command)
(define-key init-leader-map (kbd "5 o") #'other-frame)
(define-key init-leader-map (kbd "5 u") #'undelete-frame)
(define-key init-leader-map (kbd "t 0") #'tab-bar-close-tab)
(define-key init-leader-map (kbd "t 1") #'tab-bar-close-group-tabs)
(define-key init-leader-map (kbd "t 2") #'tab-bar-new-tab)
(define-key init-leader-map (kbd "t o") #'tab-bar-switch-to-next-tab)
(define-key init-leader-map (kbd "t O") #'tab-bar-switch-to-prev-tab)
(define-key init-leader-map (kbd "t u") #'tab-bar-undo-close-tab)

(define-key init-leader-map (kbd "r m") #'bookmark-set)
(define-key init-leader-map (kbd "r b") #'helm-bookmarks)
(define-key init-leader-map (kbd "r e") #'helm-recentf)
(define-key init-leader-map (kbd "r w") #'org-store-link)
(define-key init-leader-map (kbd "r a") #'org-agenda)
(define-key init-leader-map (kbd "r c") #'org-capture)
(define-key init-leader-map (kbd "r A") #'helm-org-agenda-files-headings)
(define-key init-leader-map (kbd "r C") #'helm-org-capture-templates)
(define-key init-leader-map (kbd "r n") #'helm-roam)

(define-key init-leader-map (kbd "x g") #'revert-buffer-quick)
(define-key init-leader-map (kbd "x G") #'revert-buffer)
(define-key init-leader-map (kbd "x v") #'vc-refresh-state)
(define-key init-leader-map (kbd "x f") #'font-lock-update)
(define-key init-leader-map (kbd "x o") #'init-open-files)
(define-key init-leader-map (kbd "x m") #'bm-toggle)
(define-key init-leader-map (kbd "x n") #'bm-next)
(define-key init-leader-map (kbd "x p") #'bm-previous)
(define-key init-leader-map (kbd "x <left>") #'previous-buffer)
(define-key init-leader-map (kbd "x <right>") #'next-buffer)

(define-key init-leader-map (kbd "p p") #'projectile-switch-project)
(define-key init-leader-map (kbd "p i") #'projectile-invalidate-cache)
(define-key init-leader-map (kbd "p f") #'projectile-find-file)
(define-key init-leader-map (kbd "p b") #'projectile-switch-to-buffer)
(define-key init-leader-map (kbd "p j") #'projectile-dired)
(define-key init-leader-map (kbd "4 p f") #'projectile-find-file-other-window)
(define-key init-leader-map (kbd "4 p b") #'projectile-switch-to-buffer-other-window)
(define-key init-leader-map (kbd "4 p j") #'projectile-dired-other-window)
(define-key init-leader-map (kbd "p s") #'projectile-save-project-buffers)
(define-key init-leader-map (kbd "p k") #'projectile-kill-buffers)
(define-key init-leader-map (kbd "p x") #'projectile-run-command-in-root)
(define-key init-leader-map (kbd "p c") #'projectile-compile-project)
(define-key init-leader-map (kbd "p !") #'projectile-run-shell-command-in-root)
(define-key init-leader-map (kbd "p &") #'projectile-run-async-shell-command-in-root)
(define-key init-leader-map (kbd "p v") #'projectile-vc)
(define-key init-leader-map (kbd "p g") #'projectile-ripgrep)

(define-key init-leader-map (kbd "v v") #'magit-status)
(define-key init-leader-map (kbd "v V") #'magit-dispatch)
(define-key init-leader-map (kbd "v ?") #'magit-file-dispatch)
(define-key init-leader-map (kbd "v g") #'magit-status-here)
(define-key init-leader-map (kbd "v G") #'magit-display-repository-buffer)
(define-key init-leader-map (kbd "v s") #'magit-stage-buffer-file)
(define-key init-leader-map (kbd "v u") #'magit-unstage-buffer-file)
(define-key init-leader-map (kbd "v d") #'magit-diff-buffer-file)
(define-key init-leader-map (kbd "v D") #'magit-diff)
(define-key init-leader-map (kbd "v l") #'magit-log-buffer-file)
(define-key init-leader-map (kbd "v L") #'magit-log)
(define-key init-leader-map (kbd "v b") #'magit-blame-addition)
(define-key init-leader-map (kbd "v B") #'magit-blame)
(define-key init-leader-map (kbd "v f") #'magit-find-file)
(define-key init-leader-map (kbd "v F") #'magit-blob-visit-file)
(define-key init-leader-map (kbd "v n") #'magit-blob-next)
(define-key init-leader-map (kbd "v p") #'magit-blob-previous)
(define-key init-leader-map (kbd "v t") #'git-timemachine)
(define-key init-leader-map (kbd "v [") #'diff-hl-previous-hunk)
(define-key init-leader-map (kbd "v ]") #'diff-hl-next-hunk)
(define-key init-leader-map (kbd "v {") #'diff-hl-show-hunk-previous)
(define-key init-leader-map (kbd "v }") #'diff-hl-show-hunk-next)
(define-key init-leader-map (kbd "v *") #'diff-hl-show-hunk)
(define-key init-leader-map (kbd "v =") #'diff-hl-diff-goto-hunk)
(define-key init-leader-map (kbd "v S") #'diff-hl-stage-dwim)
(define-key init-leader-map (kbd "v x") #'diff-hl-revert-hunk)

(define-key init-leader-map (kbd "e") #'eshell-dwim)
(define-key init-leader-map (kbd "p e") #'eshell-dwim-project)

(define-key init-leader-map (kbd "n w") #'widen)
(define-key init-leader-map (kbd "n n") #'narrow-to-region)
(define-key init-leader-map (kbd "n d") #'narrow-to-defun)
(define-key init-leader-map (kbd "n p") #'narrow-to-page)

(define-key init-leader-map (kbd "g g") #'rg-menu)
(define-key init-leader-map (kbd "g d") #'rg-dwim)
(define-key init-leader-map (kbd "g c") #'rg-dwim-current-dir)
(define-key init-leader-map (kbd "g f") #'rg-dwim-current-file)
(define-key init-leader-map (kbd "g o") #'occur)
(define-key init-leader-map (kbd "g n") #'next-error)
(define-key init-leader-map (kbd "g p") #'previous-error)

(define-key init-leader-map (kbd "s") #'helm-occur)
(define-key init-leader-map (kbd "i") #'helm-imenu)
(define-key init-leader-map (kbd "I") #'helm-imenu-in-all-buffers)
(define-key init-leader-map (kbd "S") #'helm-do-grep-ag)
(define-key init-leader-map (kbd "F") #'helm-find)

(define-key init-leader-map (kbd "=") #'format-all-region-or-buffer)
(define-key init-leader-map (kbd "%") #'query-replace-regexp)
(define-key init-leader-map (kbd ".") #'xref-find-definitions)
(define-key init-leader-map (kbd "?") #'xref-find-references)
(define-key init-leader-map (kbd ",") #'xref-go-back)
(define-key init-leader-map (kbd "4 .") #'xref-find-definitions-other-window)
(define-key init-leader-map (kbd "(") #'sp-wrap-round)
(define-key init-leader-map (kbd "[") #'sp-wrap-square)
(define-key init-leader-map (kbd "{") #'sp-wrap-curly)

(define-key init-leader-map (kbd "m a") #'auto-save-visited-mode)
(define-key init-leader-map (kbd "m A") #'auto-revert-mode)
(define-key init-leader-map (kbd "m t") #'toggle-truncate-lines)
(define-key init-leader-map (kbd "m l") #'display-line-numbers-mode)
(define-key init-leader-map (kbd "m L") #'init-toggle-line-numbers-type)
(define-key init-leader-map (kbd "m h") #'hl-line-mode)
(define-key init-leader-map (kbd "m s") #'whitespace-mode)
(define-key init-leader-map (kbd "m v") #'visual-line-mode)
(define-key init-leader-map (kbd "m s") #'lsp)
(define-key init-leader-map (kbd "m c") #'flycheck-mode)

(define-key init-leader-map (kbd "h h") #'help-for-help)
(define-key init-leader-map (kbd "h .") #'display-local-help)
(define-key init-leader-map (kbd "h i") #'info)
(define-key init-leader-map (kbd "4 h i") #'info-other-window)
(define-key init-leader-map (kbd "h l") #'view-lossage)
(define-key init-leader-map (kbd "h e") #'view-echo-area-messages)
(define-key init-leader-map (kbd "h d") #'dashboard-open)
(define-key init-leader-map (kbd "h s") #'scratch-buffer)
(define-key init-leader-map (kbd "h o") #'helm-apropos)
(define-key init-leader-map (kbd "h x") #'helpful-command)
(define-key init-leader-map (kbd "h f") #'helpful-function)
(define-key init-leader-map (kbd "h v") #'helpful-variable)
(define-key init-leader-map (kbd "h p") #'describe-package)
(define-key init-leader-map (kbd "h P") #'helm-packages)
(define-key init-leader-map (kbd "h m") #'describe-mode)
(define-key init-leader-map (kbd "h b") #'helm-descbinds)
(define-key init-leader-map (kbd "h B") #'describe-keymap)
(define-key init-leader-map (kbd "h w") #'where-is)
(define-key init-leader-map (kbd "h k") #'helpful-key)
(define-key init-leader-map (kbd "h c") #'describe-key-briefly)
(define-key init-leader-map (kbd "h t l") #'load-library)
(define-key init-leader-map (kbd "h t f") #'load-file)
(define-key init-leader-map (kbd "h t t") #'helm-themes)
(define-key init-leader-map (kbd "h L") #'helm-locate-library)
(define-key init-leader-map (kbd "h F") #'find-function)
(define-key init-leader-map (kbd "h V") #'find-variable)
(define-key init-leader-map (kbd "h K") #'find-function-on-key)
(define-key init-leader-map (kbd "4 h L") #'find-library-other-window)
(define-key init-leader-map (kbd "4 h F") #'find-function-other-window)
(define-key init-leader-map (kbd "4 h V") #'find-variable-other-window)
(define-key init-leader-map (kbd "4 h K") #'find-function-on-key-other-window)

;;; end

(provide 'init-emacs)
;;; init-emacs.el ends here
