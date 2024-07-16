;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'cl-lib)

(defvar init-directory (expand-file-name "emacs-init" user-emacs-directory))

(defvar init-lisp-directory (expand-file-name "lisp" init-directory))
(defvar init-site-directory (expand-file-name "site" init-directory))
(defvar init-misc-directory (expand-file-name "misc" init-directory))

(add-to-list 'load-path init-lisp-directory)
(add-to-list 'load-path init-site-directory)

(defvar init-enabled-modules
  '(init-emacs
    ;; init-clojure
    ;; init-python
    ;; init-pyim
    ;; init-roam
    ))

(defvar init-module-metadata
  '((init-emacs . ((site evil-x eshell-x)
                   (elpa
                    gcmh bm page-break-lines dashboard swiper
                    evil evil-collection evil-surround evil-snipe evil-multiedit
                    smartparens rainbow-delimiters evil-cleverparens
                    company yasnippet yasnippet-snippets
                    helm helm-comint helm-descbinds helm-themes
                    helm-ls-git projectile helm-projectile
                    with-editor git-modes magit forge git-timemachine
                    diredfl rg wgrep wgrep-helm
                    format-all flycheck
                    embark god-mode
                    helpful macrostep
                    markdown-mode edit-indirect
                    htmlize evil-org helm-org orgit orgit-forge)))
    (init-clojure . ((elpa clojure-mode cider clj-refactor helm-cider)))
    (init-python . ((elpa elpy python-mls)))
    (init-pyim . ((elpa pyim pyim-basedict posframe popon)))
    (init-roam . ((site helm-roam)
                  (elpa org-roam org-roam-ui)))))

(eval-and-compile
  (defvar init-override-variables nil))

(defmacro setq! (sym val)
  (let ((val (or (cdr (assq sym init-override-variables)) val)))
    `(eval-and-compile
       (progn
         (defvar ,sym)
         (setq ,sym ,val)))))

(setq! package-archives
       '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq! package-quickstart t)

(defun init-packages (type)
  (cl-remove-duplicates
   (apply #'append
          (mapcar
           (lambda (module)
             (let ((metadata (cdr (assq module init-module-metadata))))
               (cdr (assq type metadata))))
           init-enabled-modules))))

(defun init-site-packages () (init-packages 'site))
(defun init-elpa-packages () (init-packages 'elpa))

(defun init-install-elpa-packages ()
  (interactive)
  (dolist (package (init-elpa-packages))
    (package-install package)))

(defun init-compile-site-packages ()
  (interactive)
  (dolist (package (init-site-packages))
    (byte-compile-file
     (expand-file-name
      (concat (symbol-name package) ".el")
      init-site-directory))))

(defun init-compile-modules ()
  (interactive)
  (dolist (module (cons 'init-core init-enabled-modules))
    (byte-compile-file
     (expand-file-name
      (concat (symbol-name module) ".el")
      init-lisp-directory))))

(defun init-compile ()
  (interactive)
  (init-compile-site-packages)
  (init-compile-modules))

(defun init-load-modules ()
  (dolist (module init-enabled-modules)
    (require module)))

(provide 'init-core)
