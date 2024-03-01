;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)
(require 'init-basic-compl)


;;; helpful

(init-setq-declare!
 helpful-max-buffers nil
 helm-describe-function-function #'helpful-callable
 helm-describe-variable-function #'helpful-variable)


;;; dired

(init-setq-declare!
 dired-dwim-target t
 dired-listing-switches "-lha")

(with-eval-after-load 'dired
  (put 'dired-jump 'repeat-map nil))


;;; ibuffer

(init-setq-declare!
 ibuffer-formats
 '((mark modified read-only locked
         " " (name 40 40 :left :elide)
         " " (size 9 -1 :right)
         " " (mode 16 16 :left :elide) " " filename-and-process)
   (mark " " (name 16 -1) " " filename)))


;;; flymake

(defvar flymake-mode-map)

(declare-function flymake-goto-next-error "flymake")
(declare-function flymake-goto-prev-error "flymake")

(with-eval-after-load 'flymake
  (init-define-key
   flymake-mode-map
   "M-n" #'flymake-goto-next-error
   "M-p" #'flymake-goto-prev-error))

(declare-function flymake-proc-legacy-flymake "flymake-proc")

(with-eval-after-load 'flymake-proc
  (init-remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))


;;; eldoc

(setq eldoc-minor-mode-string nil)


;;; eglot

(init-setq-declare!
 eglot-events-buffer-size 0
 eglot-extend-to-xref t
 eglot-stay-out-of '(company)
 eglot-ignored-server-capabilities
 '(:hoverProvider :documentHighlightProvider))


;;; ediff

(declare-function ediff-setup-windows-plain "ediff")

(init-setq-declare!
 ediff-window-setup-function #'ediff-setup-windows-plain)


;;; grep

(init-setq-declare!
 xref-search-program 'ripgrep)

(init-setq-declare!
 wgrep-auto-save-buffer t
 wgrep-change-readonly-file t)


;;; magit

(init-setq-declare!
 magit-bind-magit-project-status nil
 magit-define-global-key-bindings nil)


;;; eshell

(defun init-eshell-export-pager ()
  (setenv "PAGER" (init-expand-misc-file-name "pager.py")))

(init-add-hook 'eshell-mode-hook (list #'init-eshell-export-pager #'with-editor-export-editor))

(init-add-advice :override 'evil-collection-eshell-escape-stay #'ignore)

(add-to-list 'init-company-enabled-modes 'eshell-mode)
(add-to-list 'init-company-backends-alist '(eshell-mode . (company-files)))

(defvar eshell-mode-map)

(defun init-eshell-remap-helm-pcomplete ()
  (init-define-key eshell-mode-map [remap completion-at-point] #'helm-esh-pcomplete))

(init-add-hook 'eshell-mode-hook #'init-eshell-remap-helm-pcomplete)


;;; ispell

(init-setq-declare!
 ispell-dictionary "american")

(provide 'init-basic-tools)
