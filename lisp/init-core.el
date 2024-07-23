;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'cl-lib)

;;; directories

(defvar init-directory (expand-file-name "emacs-init" user-emacs-directory))

(defvar init-lisp-directory (expand-file-name "lisp" init-directory))
(defvar init-misc-directory (expand-file-name "misc" init-directory))

(add-to-list 'load-path init-lisp-directory)

;;; deps

(defvar init-deps
  '((init-emacs
     ;; essentials
     (dash :type elpa)
     (s :type elpa)
     (f :type elpa)
     (gcmh :type vc :url "https://github.com/emacsmirror/gcmh")
     (embark :type elpa)
     ;; ui
     (dashboard :type elpa)
     ;; lines
     (page-break-lines :type vc :url "https://github.com/emacsmirror/page-break-lines")
     (ws-butler :type vc :url "https://github.com/emacsmirror/ws-butler")
     (bm :type vc :url "https://github.com/emacsmirror/bm")
     ;; parens
     (smartparens :type elpa)
     (rainbow-delimiters :type vc :url "https://github.com/emacsmirror/rainbow-delimiters")
     ;; evil
     (evil :type elpa)
     (evil-collection :type elpa)
     (evil-surround :type elpa)
     (evil-snipe :type vc :url "https://github.com/emacsmirror/evil-snipe")
     (evil-multiedit :type vc :url "https://github.com/emacsmirror/evil-multiedit")
     ;; helm
     (helm :type elpa)
     (helm-ls-git :type elpa)
     (helm-descbinds :type elpa)
     (helm-comint :type vc :url "https://github.com/emacsmirror/helm-comint")
     (helm-themes :type vc :url "https://github.com/emacsmirror/helm-themes")
     ;; search
     (swiper :type elpa)
     ;; help
     (elisp-refs :type vc :url "https://github.com/emacsmirror/elisp-refs")
     (helpful :type vc :url "https://github.com/emacsmirror/helpful")
     ;; project
     (projectile :type elpa)
     (helm-projectile :type elpa)
     ;; prog
     (company :type elpa)
     (yasnippet :type elpa)
     (yasnippet-snippets :type elpa)
     (flycheck :type elpa)
     (format-all :type elpa)
     (lsp-mode :type elpa)
     (lsp-ui :type elpa)
     (helm-lsp :type elpa)
     ;; tools
     (ibuffer-vc :type vc :url "https://github.com/emacsmirror/ibuffer-vc")
     (diredfl :type vc :url "https://github.com/emacsmirror/diredfl")
     (git-modes :type elpa)
     (with-editor :type elpa)
     (magit :type elpa)
     (git-timemachine :type vc :url "https://github.com/emacsmirror/git-timemachine")
     (diff-hl :type vc :url "https://github.com/emacsmirror/diff-hl")
     (rg :type elpa)
     (wgrep :type elpa)
     (wgrep-helm :type elpa)
     (eshell-dwim :type vc :url "https://github.com/vhqr0/eshell-dwim")
     ;; lisp
     (evil-cleverparens :type elpa)
     (macrostep :type elpa)
     (package-lint :type elpa)
     (flycheck-package :type elpa)
     ;; markdown
     (markdown-mode :type elpa)
     (edit-indirect :type elpa)
     ;; org
     (htmlize :type elpa)
     (evil-org :type elpa)
     (helm-org :type elpa)
     (orgit :type elpa)
     ;; leaders
     (god-mode :type vc :url "https://github.com/emacsmirror/god-mode"))
    (init-python
     (python-mls :type vc :url "https://github.com/emacsmirror/python-mls")
     (lsp-pyright :type elpa))
    (init-hylang
     (hy-mode :type vc :url "https://github.com/vhqr0/hy-mode"))
    (init-clojure
     (clojure-mode :type elpa)
     (cider :type elpa)
     (clj-refactor :type elpa)
     (helm-cider :type elpa))
    (init-pyim
     (pyim :type elpa)
     (pyim-basedict :type elpa)
     (posframe :type elpa)
     (popon :type elpa))
    (init-roam
     (org-roam :type elpa)
     (org-roam-ui :type elpa)
     (helm-roam :type vc :url "https://github.com/vhqr0/helm-roam"))))

(defvar init-enabled-modules
  '(init-emacs
    ;; init-python
    ;; init-hylang
    ;; init-clojure
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

(require 'package)
(require 'package-vc)

(defun init-install-package (package)
  (if (eq (plist-get (cdr package) :type) 'elpa)
      (package-install (car package))
    (package-vc-install package)))

(defun init-select-packages ()
  (interactive)
  (dolist (package (init-required-packages))
    (if (eq (plist-get (cdr package) :type) 'elpa)
        (add-to-list 'package-selected-packages (car package))
      (add-to-list 'package-vc-selected-packages package)))
  (custom-save-all))

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
