;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'cl-macs)
  (require 'init-core-macs))

(require 'init-core-vars)
(require 'init-core-utils)

(setq-declare! package
  package-quickstart t
  package-enable-at-startup nil
  package-archives init--elpa-archives)

(defmacro init--use-package! (list &rest packages)
  (declare (indent 1))
  `(progn
     ,@ (cl-loop for package in packages
                 collect `(add-to-list ',list ',package))))

(defmacro use-site-package! (&rest packages)
  `(init--use-package! init--site-packages ,@packages))

(defmacro use-elpa-package! (&rest packages)
  `(init--use-package! init--elpa-packages ,@packages))

(defun init--module-name (module)
  (concat "init-" (symbol-name module)))

(defun init--module-al-name (module)
  (concat "init-" (symbol-name module) "-al"))

(defun init--module-pkg-name (module)
  (concat "init-" (symbol-name module) "-pkg"))

(defun init--module-require (module)
  (require (intern (init--module-name module))))

(defun init--module-pkg-require (module)
  (require (intern (init--module-pkg-name module))))

(defun init--module-load ()
  (dolist (module init--modules)
    (init--module-require module)))

(defun init--module-load-for-meta ()
  (dolist (core init--cores)
    (add-to-list 'init--core-paths
                 (init--expand-lisp-file-name (concat (init--module-name core) ".el"))))
  (dolist (module init--modules)
    (let ((path     (init--expand-lisp-file-name (concat (init--module-name     module) ".el")))
          (al-path  (init--expand-lisp-file-name (concat (init--module-al-name  module) ".el")))
          (pkg-path (init--expand-lisp-file-name (concat (init--module-pkg-name module) ".el"))))
      (unless (file-exists-p path)
        (error (format "init--module-read-meta: module not found: %s"
                       (symbol-name module))))
      (add-to-list 'init--module-paths path)
      (when (file-exists-p al-path)
        (add-to-list 'init--module-al-paths al-path))
      (when (file-exists-p pkg-path)
        (add-to-list 'init--module-pkg-paths pkg-path)
        (init--module-pkg-require module))))
  (dolist (package init--site-packages)
    (add-to-list 'init--site-paths
                 (init--expand-site-file-name (concat (symbol-name package) ".el")))))

(defun init--lisp-paths ()
  (append init--core-paths
          init--module-paths
          init--module-al-paths
          init--module-pkg-paths
          init--site-paths))

(defun init--elpa-install (&optional packages)
  (require 'package)
  (package-read-all-archive-contents)
  (unless package-archive-contents
    (package-refresh-contents))
  (let ((packages (or packages init--elpa-packages)))
    (dolist (package packages)
      (unless (package-installed-p package)
        (package-install package)))))

(provide 'init-core-module)
