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
     (dash :type elpa)
     ;; edit
     (embark :type elpa)
     ;; visual
     (page-break-lines :type elpa)
     (rainbow-delimiters :type elpa)
     (rainbow-identifiers :type elpa)
     (goggles :type elpa)
     ;; parens
     (smartparens :type elpa)
     ;; evil
     (avy :type elpa)
     (ace-window :type elpa)
     (expand-region :type elpa)
     (evil :type elpa)
     (evil-collection :type elpa)
     (evil-surround :type elpa)
     (evil-snipe :type elpa)
     (evil-goggles :type elpa)
     ;; completion
     (amx :type elpa)
     (ivy :type elpa)
     (ivy-avy :type elpa)
     (ivy-hydra :type elpa)
     (swiper :type elpa)
     (counsel :type elpa)
     ;; project
     (projectile :type elpa)
     (counsel-projectile :type elpa)
     ;; prog
     (yasnippet :type elpa)
     (yasnippet-snippets :type elpa)
     (ivy-yasnippet :type elpa)
     (company :type elpa)
     (flycheck :type elpa)
     (apheleia :type elpa)
     (lsp-mode :type elpa)
     (lsp-ui :type elpa)
     ;; tools
     (with-editor :type elpa)
     (git-modes :type elpa)
     (magit :type elpa)
     (orgit :type elpa)
     (rg :type elpa)
     (wgrep :type elpa)
     (eshell-dwim :type vc :url "https://github.com/vhqr0/eshell-dwim")
     ;; elisp
     (macrostep :type elpa)
     (package-lint :type elpa)
     (flycheck-package :type elpa)
     ;; markdown
     (markdown-mode :type elpa)
     (edit-indirect :type elpa)
     ;; org
     (htmlize :type elpa)
     ;; leaders
     (god-mode :type elpa))
    (init-python
     (lsp-pyright :type elpa))
    (init-clojure
     (clojure-mode :type elpa)
     (flycheck-clj-kondo :type elpa)
     (cider :type elpa))
    (init-pyim
     (pyim :type elpa)
     (pyim-basedict :type elpa)
     (posframe :type elpa)
     (popon :type elpa))
    (init-roam
     (org-roam :type elpa)
     (org-roam-ui :type elpa))))

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

(setq! package-quickstart t)

(require 'package)
(require 'package-vc)

(defun init-install-package (package)
  "Install PACKAGE."
  (if (eq (plist-get (cdr package) :type) 'elpa)
      (package-install (car package))
    (package-vc-install package)))

(defun init-select-packages ()
  "Add required packages to selected packages."
  (interactive)
  (dolist (package (init-required-packages))
    (if (eq (plist-get (cdr package) :type) 'elpa)
        (add-to-list 'package-selected-packages (car package))
      (add-to-list 'package-vc-selected-packages package)))
  (custom-save-all))

;;; commands

(defun init-install (&optional force)
  "Install all required packages.  FORCE reinstall."
  (interactive "P")
  (dolist (package (init-required-packages))
    (when (or force (not (package-installed-p (car package))))
      (init-install-package package))))

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
