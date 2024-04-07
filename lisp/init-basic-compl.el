;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)
(require 'init-basic-emacs)

(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

(setq isearch-lazy-count t
      isearch-allow-scroll t
      isearch-allow-motion t
      isearch-yank-on-move t
      isearch-motion-changes-direction t
      isearch-repeat-on-direction-change t)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(init-global-set-key "M-/" #'hippie-expand)


;;; yasnippet

(init-setq-declare!
 yas-alias-to-yas/prefix-p nil)

(init-eval-after-init!
 (yas-global-mode 1))

(with-eval-after-load 'yasnippet
  (init-diminish-minor-mode 'yas-minor-mode))

(defun init-auto-save-visited-predicate-for-yas ()
  (and (bound-and-true-p yas-minor-mode)
       (bound-and-true-p yas--active-snippets)))

(init-add-hook 'init-auto-save-visited-predicate-hook #'init-auto-save-visited-predicate-for-yas)

(init-global-set-key
 "C-c y n" #'yas-new-snippet
 "C-c y s" #'yas-insert-snippet
 "C-c y v" #'yas-visit-snippet-file
 "C-c y w" #'aya-create
 "C-c y y" #'aya-expand
 "C-c y b" #'aya-expand-from-history)


;;; company

(init-setq-declare!
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

(defvar init-company-enabled-modes nil)
(defvar init-company-backends-alist nil)

(defun init-override-company-mode-on ()
  (when (or (derived-mode-p 'prog-mode)
            (memq major-mode init-company-enabled-modes))
    (when-let (backends (assq major-mode init-company-backends-alist))
      (setq-local company-backends (cdr backends)))
    (company-mode 1)))

(init-add-advice :override 'company-mode-on #'init-override-company-mode-on)

(init-eval-after-init!
 (global-company-mode 1))

(defun init-auto-save-visited-predicate-for-company ()
  (and (bound-and-true-p company-mode)
       (bound-and-true-p company-candidates)))

(init-add-hook 'init-auto-save-visited-predicate-hook #'init-auto-save-visited-predicate-for-company)

(init-global-set-key "C-c TAB" #'company-complete)

(provide 'init-basic-compl)
