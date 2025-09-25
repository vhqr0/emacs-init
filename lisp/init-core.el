;;; init-core.el --- Core Framework -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Core framework for Emacs init files.

;;; Code:

;;; directories

(defvar init-directory (expand-file-name "emacs-init" user-emacs-directory))
(defvar priv-directory (expand-file-name "emacs-priv" user-emacs-directory))

(defvar init-lisp-directory (expand-file-name "lisp" init-directory))
(defvar init-misc-directory (expand-file-name "misc" init-directory))

(add-to-list 'load-path init-lisp-directory)

;;; deps

(defvar init-deps
  '((init-emacs . (evil
                   evil-surround
                   embark
                   paredit
                   orderless
                   marginalia
                   vertico
                   consult
                   embark-consult
                   yasnippet
                   company
                   wgrep
                   with-editor
                   git-modes
                   magit
                   orgit
                   markdown-mode
                   edit-indirect))
    (init-clojure . (clojure-mode cider))
    (init-pyim . (pyim pyim-basedict posframe))
    (init-roam . (org-roam))))

(defvar init-modules
  '(init-emacs
    ;; init-clojure
    ;; init-python
    ;; init-pyim
    ;; init-roam
    ;; init-wsl
    ;; init-win
    ))

(defun init-packages ()
  "Return required packages based on `init-deps' and `init-modules'."
  (seq-mapcat
   (lambda (module) (cdr (assq module init-deps)))
   init-modules))

(defun init-module-files ()
  "Return module files based on `init-modules'."
  (seq-map
   (lambda (module)
     (expand-file-name (concat (symbol-name module) ".el") init-lisp-directory))
   (cons 'init-core init-modules)))

;;; commands

(require 'custom)
(require 'cus-edit)
(require 'package)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

(setq package-quickstart t)

(defun init-select-packages ()
  "Add required packages to selected packages."
  (interactive)
  (dolist (package (init-packages))
    (add-to-list 'package-selected-packages package))
  (custom-save-all))

(defun init-install ()
  "Install all required packages."
  (interactive)
  (dolist (package (init-packages))
    (package-install package)))

(defun init-compile ()
  "Compile all module files."
  (interactive)
  (dolist (file (init-module-files))
    (byte-compile-file file)))

(defun init-recompile (&optional force)
  "Recompile all module files.  FORCE recompile."
  (interactive "P")
  (dolist (file (init-module-files))
    (byte-recompile-file file force)))

(defun init-load ()
  "Load all module files."
  (dolist (module init-modules)
    (require module)))

(provide 'init-core)
;;; init-core.el ends here
