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
  '((init-emacs
     dash
     s
     f
     ;; ui
     posframe
     ;; edit
     embark
     paredit
     iedit
     rainbow-delimiters
     rainbow-identifiers
     ;; evil
     evil
     evil-collection
     evil-surround
     evil-snipe
     ;; completion
     orderless
     marginalia
     vertico
     consult
     embark-consult
     ;; prog
     apheleia
     company
     tempel
     ;; tools
     wgrep
     with-editor
     git-modes
     magit
     orgit
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
     cider)
    (init-pyim
     pyim
     pyim-basedict)
    (init-roam
     org-roam
     org-roam-ui)))

(defvar init-modules
  '(init-emacs
    ;; init-clojure
    ;; init-pyim
    ;; init-roam
    ;; init-wsl
    ))

(defun init-packages ()
  "Return required packages based on `init-deps' and `init-modules'."
  (seq-mapcat
   (lambda (module) (cdr (assq module init-deps)))
   init-modules))

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
  (dolist (package (init-packages))
    (add-to-list 'package-selected-packages package))
  (custom-save-all))

;;; commands

(defun init-install (&optional force)
  "Install all required packages.  FORCE reinstall."
  (interactive "P")
  (dolist (package (init-packages))
    (when (or force (not (package-installed-p package)))
      (package-install package))))

(defun init-compile (&optional force)
  "Compile all module files.  FORCE recompile."
  (interactive "P")
  (let ((compile-function (if force #'byte-compile-file #'byte-recompile-file)))
    (dolist (module (cons 'init-core init-modules))
      (let ((filename (expand-file-name (concat (symbol-name module) ".el") init-lisp-directory)))
        (funcall compile-function filename)))))

(defun init-load ()
  "Load all module files."
  (dolist (module init-modules)
    (require module)))

(provide 'init-core)
;;; init-core.el ends here
