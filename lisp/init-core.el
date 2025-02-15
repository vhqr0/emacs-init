;;; init-core.el --- Core Framework -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Core framework for Emacs init files.

;;; Code:

(require 'cl-lib)

;;; directories

(defvar init-directory (expand-file-name "emacs-init" user-emacs-directory))
(defvar priv-directory (expand-file-name "emacs-priv" user-emacs-directory))

(defvar init-lisp-directory (expand-file-name "lisp" init-directory))
(defvar init-misc-directory (expand-file-name "misc" init-directory))

(add-to-list 'load-path init-lisp-directory)

;;; deps

(defvar init-deps
  '((init-emacs
     dash
     ;; ui
     posframe
     ;; edit
     keycast
     embark
     paredit
     goggles
     page-break-lines
     rainbow-delimiters
     rainbow-identifiers
     ;; evil
     evil
     evil-collection
     evil-surround
     evil-snipe
     evil-goggles
     ;; completion
     orderless
     marginalia
     vertico
     corfu
     cape
     consult
     embark-consult
     ;; prog
     tempel
     apheleia
     breadcrumb
     ;; tools
     with-editor
     git-modes
     magit
     orgit
     wgrep
     ;; bindings
     god-mode
     ;; lang
     ;;; elisp
     package-lint
     package-lint-flymake
     macrostep
     ;;; org
     htmlize
     org-contrib
     ;;; markdown
     markdown-mode
     edit-indirect)
    (init-clojure
     clojure-mode
     cider
     clj-refactor)
    (init-pyim
     pyim
     pyim-basedict)
    (init-roam
     org-roam
     org-roam-ui)))

(defvar init-enabled-modules
  '(init-emacs
    ;; init-python
    ;; init-clojure
    ;; init-pyim
    ;; init-roam
    ;; init-wsl
    ))

(defun init-required-packages ()
  "Return required packages based on `init-deps' and `init-enabled-modules'."
  (cl-delete-duplicates
   (apply #'append
          (mapcar
           (lambda (module)
             (cdr (assq module init-deps)))
           init-enabled-modules))
   :test #'equal))

;;; packages

(require 'package)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

(setq package-check-signature nil)

(setq package-quickstart t)

(defun init-select-packages ()
  "Add required packages to selected packages."
  (interactive)
  (dolist (package (init-required-packages))
    (add-to-list 'package-selected-packages package))
  (custom-save-all))

;;; commands

(defun init-install (&optional force)
  "Install all required packages.  FORCE reinstall."
  (interactive "P")
  (dolist (package (init-required-packages))
    (when (or force (not (package-installed-p package)))
      (package-install package))))

(defun init-compile (&optional force)
  "Compile all module files.  FORCE recompile."
  (interactive "P")
  (let ((compile-function (if force #'byte-compile-file #'byte-recompile-file)))
    (dolist (module (cons 'init-core init-enabled-modules))
      (let ((filename (expand-file-name (concat (symbol-name module) ".el") init-lisp-directory)))
        (funcall compile-function filename)))))

(defun init-load ()
  "Load all module files."
  (dolist (module init-enabled-modules)
    (require module)))

(provide 'init-core)
;;; init-core.el ends here
