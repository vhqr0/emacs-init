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

(setq! system-time-locale "C")

(setq! read-process-output-max (* 1024 1024))

(setq! enable-recursive-minibuffers t)

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

(global-set-key (kbd "C-S-T") #'tab-bar-duplicate-tab)
(global-set-key (kbd "C-S-W") #'tab-bar-close-tab)

(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "C-=") #'text-scale-increase)

;;;; lines

(setq-default truncate-lines t)

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

;;; edit

(setq-default indent-tabs-mode nil)

(setq! word-wrap-by-category t)

(setq! disabled-command-function nil)

(require 'repeat)

(repeat-mode 1)

(global-set-key (kbd "C-SPC") #'toggle-input-method)

;;; parens

(setq! sp-ignore-modes-list nil)

(require 'smartparens)
(require 'smartparens-config)

(sp-local-pair 'minibuffer-mode "'" nil :actions nil)

(init-diminish-minor-mode 'smartparens-mode)

(smartparens-global-mode 1)
(show-smartparens-global-mode 1)

(define-key smartparens-mode-map (kbd "C-M-f") #'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") #'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "C-M-d") #'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") #'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-n") #'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") #'sp-previous-sexp)
(define-key smartparens-mode-map (kbd "C-M-k") #'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") #'sp-copy-sexp)
(define-key smartparens-mode-map (kbd "C-M-SPC") #'sp-mark-sexp)
(define-key smartparens-mode-map (kbd "C-k") #'sp-kill-hybrid-sexp)
(define-key smartparens-mode-map (kbd "M-r") #'sp-splice-sexp-killing-around)
(define-key smartparens-mode-map (kbd "M-R") #'sp-splice-sexp-killing-backward)
(define-key smartparens-mode-map (kbd "M-s") #'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "M-S") #'sp-split-sexp)
(define-key smartparens-mode-map (kbd "M-J") #'sp-join-sexp)
(define-key smartparens-mode-map (kbd "C-<right>") #'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") #'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-<right>") #'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-<left>") #'sp-backward-slurp-sexp)

(require 'rainbow-delimiters)
(require 'rainbow-identifiers)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-identifiers-mode)
(add-hook 'dired-mode-hook #'rainbow-identifiers-mode)

;;; evil

(require 'avy)

(global-set-key (kbd "C-'") #'avy-goto-char-timer)

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

;;; completion

(require 'embark)

(global-set-key (kbd "M-o") #'embark-act)

(setq! completion-ignore-case t)
(setq! read-buffer-completion-ignore-case t)
(setq! read-file-name-completion-ignore-case t)

(setq! isearch-lazy-count t)
(setq! isearch-allow-scroll t)
(setq! isearch-allow-motion t)
(setq! isearch-yank-on-move t)
(setq! isearch-motion-changes-direction t)
(setq! isearch-repeat-on-direction-change t)

(setq! ivy-count-format "(%d/%d) ")
(setq! ivy-use-selectable-prompt t)
(setq! ivy-use-virtual-buffers t)

(require 'amx)
(require 'ivy)
(require 'ivy-avy)
(require 'ivy-hydra)
(require 'swiper)
(require 'counsel)

(add-to-list 'ivy-completing-read-handlers-alist
             '(kill-buffer . completing-read-default))

(amx-mode 1)
(ivy-mode 1)
(counsel-mode 1)

(init-diminish-minor-mode 'ivy-mode)
(init-diminish-minor-mode 'counsel-mode)

(global-set-key (kbd "C-s") #'swiper-thing-at-point)
(global-set-key (kbd "C-c b") #'ivy-resume)

(advice-add 'describe-bindings :override #'counsel-descbinds)

(define-key ivy-minibuffer-map (kbd "C-x C-s") #'ivy-occur)
(define-key ivy-minibuffer-map (kbd "M-r") #'ivy-reverse-i-search)
(define-key counsel-find-file-map (kbd "C-l") #'counsel-up-directory)

(evil-define-key 'insert minibuffer-mode-map
  (kbd "M-r") #'previous-matching-history-element)

(evil-define-key 'insert ivy-minibuffer-map
  (kbd "M-r") #'ivy-reverse-i-search)

(evil-define-key '(insert normal) ivy-minibuffer-map
  (kbd "C-M-n") #'ivy-next-line-and-call
  (kbd "C-M-p") #'ivy-previous-line-and-call)

(evil-define-key 'normal ivy-minibuffer-map
  (kbd "gg")  #'ivy-beginning-of-buffer
  (kbd "G")   #'ivy-end-of-buffer
  (kbd "C-d") #'ivy-scroll-up-command
  (kbd "C-u") #'ivy-scroll-down-command
  (kbd "C-o") #'hydra-ivy/body)

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

(define-key counsel-mode-map [remap recentf-open] #'counsel-recentf)
(define-key counsel-mode-map [remap previous-matching-history-element] #'counsel-minibuffer-history)
(define-key counsel-mode-map [remap eshell-previous-matching-input] #'counsel-esh-history)
(define-key counsel-mode-map [remap comint-history-isearch-backward-regexp] #'counsel-shell-history)
(define-key counsel-mode-map [remap company-search-candidates] #'counsel-company)

(defun init-ivy--action-append (x)
  "Append X after point."
  (unless (eolp) (forward-char))
  (ivy--action-insert x))

(defun init-counsel--set-variable (x)
  "Set variable X."
  (counsel-set-variable (intern x)))

(ivy-add-actions t '(("a" init-ivy--action-append "append")))
(ivy-add-actions 'counsel-describe-variable '(("s" init-counsel--set-variable "set")))
(ivy-add-actions 'counsel-find-library '(("l" load-library "load")))
(ivy-configure 'counsel-minor :initial-input "")

;;; help

(require 'find-func)

(find-function-setup-keys)

(setq! helpful-max-buffers nil)

(require 'helpful)

(setq! counsel-describe-symbol-function #'helpful-symbol)
(setq! counsel-describe-variable-function #'helpful-variable)
(setq! counsel-describe-function-function #'helpful-callable)
(setq! counsel-descbinds-function #'helpful-callable)

(defun init-lookup-setup-command (command)
  "Setup COMMAND as local help command."
  (setq-local evil-lookup-func command)
  (local-set-key [remap display-local-help] command))

(defun init-lookup-setup-helpful ()
  "Setup helpful."
  (init-lookup-setup-command #'helpful-at-point))

(defvar init-lookup-helpful-mode-hooks
  '(emacs-lisp-mode-hook
    lisp-interaction-mode-hook
    ielm-mode-hook
    eshell-mode-hook
    org-mode-hook
    help-mode-hook
    helpful-mode-hook
    Info-mode-hook))

(dolist (hook init-lookup-helpful-mode-hooks)
  (add-hook hook #'init-lookup-setup-helpful))

;;; project

(setq! projectile-current-project-on-switch 'move-to-end)

(require 'projectile)

(projectile-mode 1)

(require 'counsel-projectile)

(define-key counsel-mode-map [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer)
(define-key counsel-mode-map [remap projectile-find-file] #'counsel-projectile-find-file)
(define-key counsel-mode-map [remap projectile-find-dir] #'counsel-projectile-find-dir)
(define-key counsel-mode-map [remap projectile-ripgrep] #'counsel-projectile-rg)

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

(add-hook 'prog-mode-hook #'flycheck-mode)

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

(define-key dired-mode-map "O" #'dired-omit-mode)

(evil-collection-define-key 'normal 'dired-mode-map
  "O" #'dired-omit-mode)

;;;; git

(autoload 'magit-blob-next "magit" nil t)
(autoload 'magit-blob-previous "magit" nil t)

;;;; grep

(setq! wgrep-auto-save-buffer t)
(setq! wgrep-change-readonly-file t)

(autoload 'rg-menu "rg" nil t)
(declare-function rg-menu "rg")

;;;; comint

(require 'comint)

(evil-define-key 'insert comint-mode-map
  (kbd "M-r") #'comint-history-isearch-backward-regexp)

;;;; eshell

(require 'eshell)
(require 'em-hist)

(defun init-eshell-set-company ()
  "Clean company backends."
  (setq-local company-backends '(company-files)))

(add-hook 'eshell-mode-hook #'with-editor-export-editor)
(add-hook 'eshell-mode-hook #'init-eshell-set-company)

(declare-function evil-collection-eshell-escape-stay "evil-collection-eshell")
(advice-add #'evil-collection-eshell-escape-stay :override #'ignore)

(evil-define-key 'insert eshell-hist-mode-map
  (kbd "M-r") #'eshell-previous-matching-input)

;;;; spell

(setq! ispell-dictionary "american")

;;; lang

;;;; elisp

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (define-key map (kbd "C-c e") #'macrostep-expand))

(setq! flycheck-emacs-lisp-load-path load-path)

(require 'flycheck-package)

(flycheck-package-setup)

;;;; markdown

(setq! markdown-fontify-code-blocks-natively t)

(require 'markdown-mode)
(require 'edit-indirect)

(define-key markdown-mode-map (kbd "C-c C-'") #'markdown-edit-code-block)
(define-key edit-indirect-mode-map (kbd "C-c C-'") #'edit-indirect-commit)

;;;; org

(setq! org-directory (expand-file-name "org" user-emacs-directory))
(setq! org-agenda-files (list org-directory))
(setq! org-default-notes-file (expand-file-name "inbox.org" org-directory))

(setq! org-capture-templates
       '(("t" "Task" entry (file+headline "" "Tasks") "* TODO %?\n%U\n%a")))

(require 'org)

(defun init-org-modify-syntax ()
  "Modify `org-mode' syntax table."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(add-hook 'org-mode-hook #'init-org-modify-syntax)

(define-key org-mode-map (kbd "C-c l") #'org-toggle-link-display)

(define-key org-mode-map (kbd "C-c C-'") #'org-edit-special)
(define-key org-src-mode-map (kbd "C-c C-'") #'org-edit-src-exit)
(define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

;;; leaders

(defvar init-leader-key "SPC")
(defvar init-leader-state '(motion normal visual operator))

(defun init-leader-bindings (clauses)
  "Transforms `define-key' CLAUSES to binding alist."
  (->> clauses
       (-partition 2)
       (--map
        (cons (kbd (concat init-leader-key " " (car it))) (cadr it)))))

(defun init-leader-define-key (keymap &rest clauses)
  "Define leader binding CLAUSES in KEYMAP."
  (declare (indent defun))
  (dolist (binding (init-leader-bindings clauses))
    (evil-define-key* init-leader-state keymap (car binding) (cdr binding))))

(defun init-leader-global-set-key (&rest clauses)
  "Define leader binding CLAUSES in `init-evil-override-mode-map'."
  (apply #'init-leader-define-key (cons init-evil-override-mode-map clauses)))

(defun init-leader-define-minor-mode-key (mode &rest clauses)
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

(init-leader-global-set-key
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
 "`" #'tmm-menubar)

(init-leader-global-set-key
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
 "w <right>" #'winner-redo)

(init-leader-global-set-key
 "b" #'switch-to-buffer
 "f" #'find-file
 "d" #'dired
 "j" #'dired-jump
 "k" #'kill-buffer
 "4 b" #'switch-to-buffer-other-window
 "4 f" #'find-file-other-window
 "4 d" #'dired-other-window
 "4 j" #'dired-jump-other-window
 "5 0" #'delete-frame
 "5 1" #'delete-other-frames
 "5 2" #'make-frame-command
 "5 o" #'other-frame
 "5 u" #'undelete-frame
 "5 b" #'switch-to-buffer-other-frame
 "5 f" #'find-file-other-frame
 "5 d" #'dired-other-frame
 "t 0" #'tab-bar-close-tab
 "t 1" #'tab-bar-close-group-tabs
 "t 2" #'tab-bar-new-tab
 "t o" #'tab-bar-switch-to-next-tab
 "t O" #'tab-bar-switch-to-prev-tab
 "t u" #'tab-bar-undo-close-tab
 "t b" #'switch-to-buffer-other-tab
 "t f" #'find-file-other-tab
 "t d" #'dired-other-tab)

(init-leader-global-set-key
 "r m" #'bookmark-set
 "r b" #'bookmark-jump
 "r e" #'recentf-open
 "r w" #'org-store-link
 "r a" #'org-agenda
 "r c" #'org-capture
 "r A" #'counsel-org-agenda-headlines
 "r C" #'counsel-org-capture)

(init-leader-global-set-key
 "p p" #'projectile-switch-project
 "p i" #'projectile-invalidate-cache
 "p b" #'projectile-switch-to-buffer
 "p f" #'projectile-find-file
 "p d" #'projectile-find-dir
 "p j" #'projectile-dired
 "4 p b" #'projectile-switch-to-buffer-other-window
 "4 p f" #'projectile-find-file-other-window
 "4 p d" #'projectile-find-dir-other-window
 "4 p j" #'projectile-dired-other-window
 "5 p b" #'projectile-switch-to-buffer-other-frame
 "5 p f" #'projectile-find-file-other-frame
 "5 p d" #'projectile-find-dir-other-frame
 "5 p j" #'projectile-dired-other-frame
 "p s" #'projectile-save-project-buffers
 "p k" #'projectile-kill-buffers
 "p x" #'projectile-run-command-in-root
 "p c" #'projectile-compile-project
 "p !" #'projectile-run-shell-command-in-root
 "p &" #'projectile-run-async-shell-command-in-root
 "p v" #'projectile-vc
 "p g" #'projectile-ripgrep)

(init-leader-global-set-key
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
 "v p" #'magit-blob-previous)

(init-leader-global-set-key
 "e" #'eshell-dwim
 "p e" #'eshell-dwim-project)

(init-leader-global-set-key
 "n w" #'widen
 "n n" #'narrow-to-region
 "n d" #'narrow-to-defun
 "n p" #'narrow-to-page)

(init-leader-define-key markdown-mode-map
  "n b" #'markdown-narrow-to-block
  "n s" #'markdown-narrow-to-subtree)

(init-leader-define-key org-mode-map
  "n b" #'org-narrow-to-block
  "n s" #'org-narrow-to-subtree)

(init-leader-global-set-key
  "g o" #'occur
  "g n" #'next-error
  "g p" #'previous-error
  "g g" #'rg-menu
  "g d" #'rg-dwim
  "g ;" #'avy-resume
  "g j" #'avy-goto-line
  "g f" #'avy-goto-char-timer
  "g r" #'revert-buffer-quick
  "g R" #'revert-buffer
  "g v" #'vc-refresh-state
  "g =" #'font-lock-update
  "g <left>" #'previous-buffer
  "g <right>" #'next-buffer)

(init-leader-global-set-key
  "s" #'swiper
  "S" #'swiper-all
  "/" #'swiper-from-isearch
  "l l" #'counsel-outline
  "l g" #'counsel-rg
  "l f" #'counsel-file-jump
  "l d" #'counsel-dired-jump)

(init-leader-global-set-key
  "=" #'apheleia-format-buffer
  "%" #'query-replace-regexp
  "." #'xref-find-definitions
  "?" #'xref-find-references
  "," #'xref-go-back
  "4 ." #'xref-find-definitions-other-window
  "i" #'imenu
  "(" #'sp-wrap-round
  "[" #'sp-wrap-square
  "{" #'sp-wrap-curly)

(init-leader-define-minor-mode-key 'lsp-mode
  "y" lsp-command-map)

(init-leader-global-set-key
  "m a" #'auto-save-visited-mode
  "m A" #'auto-revert-mode
  "m t" #'toggle-truncate-lines
  "m l" #'display-line-numbers-mode
  "m L" #'init-toggle-line-numbers-type
  "m h" #'hl-line-mode
  "m w" #'whitespace-mode
  "m v" #'visual-line-mode
  "m r d" #'rainbow-delimiters-mode
  "m r i" #'rainbow-identifiers-mode
  "m s" #'lsp
  "m c" #'company-mode
  "m y" #'yas-minor-mode
  "m f" #'flycheck-mode
  "m =" #'apheleia-mode
  "m m" #'counsel-minor
  "m M" #'counsel-major)

(init-leader-global-set-key
  "h h" #'help-for-help
  "h ." #'display-local-help
  "h i" #'info
  "4 h i" #'info-other-window
  "h S" #'info-lookup-symbol
  "h L" #'view-lossage
  "h e" #'view-echo-area-messages
  "h s" #'scratch-buffer
  "h a" #'apropos-command
  "h o" #'describe-symbol
  "h x" #'describe-command
  "h f" #'describe-function
  "h v" #'describe-variable
  "h p" #'describe-package
  "h m" #'describe-mode
  "h b" #'describe-bindings
  "h B" #'describe-keymap
  "h l" #'find-library
  "h w" #'where-is
  "h k" #'helpful-key
  "h c" #'describe-key-briefly
  "h t l" #'load-library
  "h t f" #'load-file
  "h t t" #'load-theme
  "4 h L" #'find-library-other-window
  "4 h F" #'find-function-other-window
  "4 h V" #'find-variable-other-window
  "4 h K" #'find-function-on-key-other-window)

;;; end

(provide 'init-emacs)
;;; init-emacs.el ends here
