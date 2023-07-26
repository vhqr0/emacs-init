;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-core-utils)
(require 'init-basic-emacs)

(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

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
  company-transformers
  '(company-sort-prefer-same-case-prefix)
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

(global-company-mode 1)

(declare-variable! company
  company-mode-map)

(define-key! company-mode
  "<f2>" #'company-complete)

(define-auto-save-visited-predicate! company
  (and (bound-and-true-p company-mode)
       (bound-and-true-p company-candidates)))

(provide 'init-basic-compl)
