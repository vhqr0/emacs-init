;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-core-utils)
(require 'init-basic-emacs)


;;; projectile

(setq-declare! projectile
  projectile-switch-project-action #'helm-projectile
  projectile-current-project-on-switch 'move-to-end)

(after-init!
 (projectile-mode 1))

(setq-declare! helm-projectile
  helm-projectile-fuzzy-match nil
  helm-projectile-truncate-lines t)

(after-load! helm
  (require 'helm-projectile))


;;; xref

(setq-declare! xref
  xref-search-program 'ripgrep)


;;; flymake

(declare-variable! flymake
  flymake-mode-map)

(declare-function! flymake
  flymake-goto-next-error
  flymake-goto-prev-error)

(after-load! flymake
  (define-key! flymake-mode
    "M-n" #'flymake-goto-next-error
    "M-p" #'flymake-goto-prev-error))

(declare-function! flymake-proc
  flymake-proc-legacy-flymake)

(after-load! flymake-proc
  (remove-hook! flymake-diagnostic-functions flymake-proc-legacy-flymake))


;;; eldoc

(setq eldoc-minor-mode-string nil)


;;; eglot

(setq-declare! eglot
  eglot-events-buffer-size 0
  eglot-extend-to-xref t
  eglot-stay-out-of '(company)
  eglot-ignored-server-capabilities
  '(:hoverProvider :documentHighlightProvider))


;;; yasnippet

(setq-declare! yasnippet
  yas-alias-to-yas/prefix-p nil)

(after-init!
 (yas-global-mode 1))

(after-load! yasnippet
  (diminish! yas-minor))

(define-auto-save-visited-predicate! yas
  (and (bound-and-true-p yas-minor-mode)
       (bound-and-true-p yas--active-snippets)))


;;; company

(setq-declare! company
  company-idle-delay 0.2
  company-minimum-prefix-length 2
  company-selection-wrap-around t
  company-show-quick-access t
  company-tooltip-width-grow-only t
  company-tooltip-align-annotations t
  company-dabbrev-downcase nil
  company-dabbrev-ignore-case t
  company-dabbrev-code-ignore-case t
  company-frontends
  '(company-pseudo-tooltip-frontend
    company-preview-if-just-one-frontend
    company-echo-metadata-frontend)
  company-backends
  '(company-files
    (company-capf :with company-yasnippet)
    (company-dabbrev-code company-keywords :with company-yasnippet)
    (company-dabbrev company-yasnippet)))

(defvar! init--company-enabled-modes nil)

(defun-add-advice! :override company-mode-on
                   init--override-company-mode-on ()
  (when (or (derived-mode-p 'prog-mode)
            (memq major-mode init--company-enabled-modes))
    (company-mode 1)))

(after-init!
 (global-company-mode 1))

(declare-variable! company
  company-mode-map)

(define-auto-save-visited-predicate! company
  (and (bound-and-true-p company-mode)
       (bound-and-true-p company-candidates)))

(defun init--normalize-company-backend (x)
  (init--symbol-add-prefix "company-" x))

(defmacro define-company-enabled-mode! (mode &rest backends)
  (declare (indent 1))
  (let* ((mode (init--normalize-mode-symbol mode))
         (name (intern (concat "init--company-set-backends-for-" (symbol-name mode))))
         (backends (mapcar #'init--normalize-company-backend backends)))
    `(progn
       (add-to-list 'init--company-enabled-modes ',mode)
       ,@(when backends
           `((defun-add-hook! ,mode ,name ()
               (setq-local company-backends ',backends)))))))

(provide 'init-basic-prog)
