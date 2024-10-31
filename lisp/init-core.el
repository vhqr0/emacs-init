;;; init-core --- Core Framework -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Core framework for Emacs init files.

;;; Code:

(require 'cl-lib)

;;; directories

(defvar init-directory (expand-file-name "emacs-init" user-emacs-directory))

(defvar init-lisp-directory (expand-file-name "lisp" init-directory))
(defvar init-misc-directory (expand-file-name "misc" init-directory))

(add-to-list 'load-path init-lisp-directory)

;;; deps

(defvar init-deps
  '((init-emacs
     dash
     ;; edit
     embark
     ;; visual
     page-break-lines
     rainbow-delimiters
     rainbow-identifiers
     goggles
     ;; parens
     smartparens
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
     consult
     embark-consult
     ;; prog
     yasnippet
     yasnippet-snippets
     consult-yasnippet
     company
     consult-company
     flycheck
     apheleia
     lsp-mode
     lsp-ui
     ;; tools
     with-editor
     git-modes
     magit
     orgit
     wgrep
     ;; leaders
     god-mode
     ;; elisp
     macrostep
     package-lint
     flycheck-package
     ;; markdown
     markdown-mode
     edit-indirect
     ;; org
     htmlize)
    (init-python
     lsp-pyright)
    (init-clojure
     clojure-mode
     flycheck-clj-kondo
     cider)
    (init-pyim
     posframe
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

;;; custom set

(eval-and-compile
  (defvar init-override-variables nil))

(defmacro setq! (sym val)
  "`setq' SYM VAL that can be overridden via `init-override-variables'."
  (let ((val (or (cdr (assq sym init-override-variables)) val)))
    `(eval-and-compile
       (progn
         (defvar ,sym)
         (setq ,sym ,val)))))

;;; packages

(setq! package-archives
       '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq! package-check-signature nil)

(setq! package-quickstart t)

(require 'package)

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
