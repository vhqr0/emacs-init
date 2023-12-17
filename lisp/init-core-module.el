;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'cl-lib)
(require 'init-core-vars)
(require 'init-core-lib)
(require 'init-core-meta)

(init-setq-declare!
 package-quickstart t
 package-enable-at-startup nil
 package-check-signature nil
 package-archives init-elpa-archives)



(defun init-all-modules ()
  (append init-core-modules init-modules))

(defun init-packages (type)
  (let (packages)
    (dolist (module init-modules)
      (when-let (meta (assq module init-metas))
        (when-let (type-packages (assq type meta))
          (dolist (package (cdr type-packages))
            (setq packages (cons package packages))))))
    (cl-remove-duplicates packages)))

(defun init-site-packages () (init-packages 'site))
(defun init-elpa-packages () (init-packages 'elpa))

(defun init-module-path (module)
  (init-expand-lisp-file-name (concat (symbol-name module) ".el")))

(defun init-site-path (site-package)
  (init-expand-site-file-name (concat (symbol-name site-package) ".el")))



(defun init-elpa-install ()
  (interactive)
  (dolist (package (init-elpa-packages))
    (package-install package)))

(defun init-site-loaddefs-generate (&optional load)
  (interactive "P")
  (let ((loaddefs-path (init-expand-site-file-name "site-autoloads.el")))
    (funcall (if (fboundp 'loaddefs-generate) #'loaddefs-generate #'make-directory-autoloads)
             init-site-directory loaddefs-path)
    (when load
      (load-file loaddefs-path))))

(defun init-site-compile (&optional recompile)
  (interactive "P")
  (dolist (package (init-site-packages))
    (funcall (if recompile #'byte-recompile-file #'byte-compile-file)
             (init-site-path package))))

(defun init-module-compile (&optional recompile)
  (interactive "P")
  (dolist (module (init-all-modules))
    (funcall (if recompile #'byte-recompile-file #'byte-compile-file)
             (init-module-path module))))

(defun init-compile ()
  (interactive)
  (init-site-loaddefs-generate t)
  (init-site-compile)
  (init-module-compile))

(defun init-recompile ()
  (interactive)
  (init-site-compile t)
  (init-module-compile t))

(defun init-module-load ()
  (interactive)
  (dolist (module init-modules)
    (require module)))

(provide 'init-core-module)
