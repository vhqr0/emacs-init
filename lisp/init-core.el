;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'cl-lib)

;;; directories

(defvar init-directory (expand-file-name "emacs-init" user-emacs-directory))

(defvar init-lisp-directory (expand-file-name "lisp" init-directory))
(defvar init-site-directory (expand-file-name "site" init-directory))
(defvar init-misc-directory (expand-file-name "misc" init-directory))

(add-to-list 'load-path init-lisp-directory)
(add-to-list 'load-path init-site-directory)

;;; deps

(defvar init-deps
  '((init-emacs
     ;; essentials
     (dash :fetcher elpa)
     (s :fetcher elpa)
     (f :fetcher elpa)
     (gcmh :fetcher github :repo "vhqr0/gcmh")
     (embark :fetcher elpa)
     ;; ui
     (dashboard :fetcher elpa)
     ;; lines
     (page-break-lines :fetcher elpa)
     (ws-butler :fetcher github :repo "vhqr0/ws-butler")
     (bm :fetcher github :repo "vhqr0/bm")
     ;; parens
     (smartparens :fetcher elpa)
     (rainbow-delimiters :fetcher github :repo "vhqr0/rainbow-delimiters")
     ;; evil
     (evil :fetcher elpa)
     (evil-collection :fetcher elpa)
     (evil-surround :fetcher elpa)
     (evil-snipe :fetcher elpa)
     (evil-multiedit :fetcher elpa)
     ;; helm
     (helm :fetcher elpa)
     (helm-ls-git :fetcher elpa)
     (helm-descbinds :fetcher elpa)
     (helm-comint :fetcher github :repo "vhqr0/helm-comint")
     (helm-themes :fetcher github :repo "vhqr0/helm-themes")
     ;; search
     (swiper :fetcher elpa)
     ;; help
     (elisp-refs :fetcher elpa)
     (helpful :fetcher elpa)
     ;; project
     (projectile :fetcher elpa)
     (helm-projectile :fetcher elpa)
     ;; prog
     (company :fetcher elpa)
     (yasnippet :fetcher elpa)
     (yasnippet-snippets :fetcher elpa)
     (flycheck :fetcher elpa)
     (format-all :fetcher elpa)
     ;; tools
     (ibuffer-vc :fetcher elpa)
     (diredfl :fetcher elpa)
     (git-modes :fetcher elpa)
     (with-editor :fetcher elpa)
     (magit :fetcher elpa)
     (git-timemachine :fetcher elpa)
     (rg :fetcher elpa)
     (wgrep :fetcher elpa)
     (wgrep-helm :fetcher elpa)
     (eshell-dwim :fetcher github :repo "vhqr0/eshell-dwim")
     ;; lisp
     (evil-cleverparens :fetcher elpa)
     (macrostep :fetcher elpa)
     (package-lint :fetcher elpa)
     (flycheck-package :fetcher elpa)
     ;; markdown
     (markdown-mode :fetcher elpa)
     (edit-indirect :fetcher elpa)
     ;; org
     (htmlize :fetcher elpa)
     (evil-org :fetcher elpa)
     (helm-org :fetcher elpa)
     (orgit :fetcher elpa)
     ;; leaders
     (god-mode :fetcher github :repo "vhqr0/god-mode"))
    (init-clojure
     (clojure-mode :fetcher elpa)
     (cider :fetcher elpa)
     (clj-refactor :fetcher elpa)
     (helm-cider :fetcher elpa))
    (init-python
     (python-mls :fetcher elpa)
     (hy-mode :fetcher github :repo "vhqr0/hy-mode"))
    (init-pyim
     (pyim :fetcher elpa)
     (pyim-basedict :fetcher elpa)
     (posframe :fetcher elpa)
     (popon :fetcher elpa))
    (init-roam
     (org-roam :fetcher elpa)
     (org-roam-ui :fetcher elpa)
     (helm-roam :fetcher github :repo "vhqr0/helm-roam"))))

(defvar init-enabled-modules
  '(init-emacs
    ;; init-clojure
    ;; init-python
    ;; init-pyim
    ;; init-roam
    ))

(defun init-required-packages ()
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
(setq! quelpa-checkout-melpa-p nil)
(setq! quelpa-update-melpa-p nil)

(require 'package)
(require 'quelpa)

(defun init-install-package (package)
  (if (eq (plist-get (cdr package) :fetcher) 'elpa)
      (package-install (car package))
    (quelpa package)))

;;; commands

(defun init-install (&optional force)
  (interactive "P")
  (dolist (package (init-required-packages))
    (when (or force (not (package-installed-p (car package))))
      (init-install-package package))))

(defun init-compile (&optional force)
  (interactive "P")
  (let ((compile-function (if force #'byte-compile-file #'byte-recompile-file)))
    (dolist (module (cons 'init-core init-enabled-modules))
      (let ((filename (expand-file-name (concat (symbol-name module) ".el") init-lisp-directory)))
        (funcall compile-function filename)))))

(defun init-load ()
  (dolist (module init-enabled-modules)
    (require module)))

(provide 'init-core)
