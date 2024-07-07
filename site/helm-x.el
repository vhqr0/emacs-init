;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'cl-lib)
(require 'helm)

(defun helm-x-history ()
  (interactive)
  (cl-case (derived-mode-p 'comint-mode 'eshell-mode 'minibuffer-mode)
    (comint-mode     (helm-comint-input-ring))
    (eshell-mode     (helm-eshell-history))
    (minibuffer-mode (helm-minibuffer-history))))

(defun helm-x-imenu ()
  (interactive)
  (cl-case (derived-mode-p 'comint-mode 'eshell-mode)
    (comint-mode (helm-comint-prompts))
    (eshell-mode (helm-eshell-prompts))
    (t           (helm-imenu))))

(defun helm-x-imenu-all ()
  (interactive)
  (cl-case (derived-mode-p 'comint-mode 'eshell-mode)
    (comint-mode (helm-comint-prompts-all))
    (eshell-mode (helm-eshell-prompts-all))
    (t           (helm-imenu-in-all-buffers))))

(declare-function project-root "project")
(declare-function helm-fd-1 "helm-fd")
(declare-function helm-grep-ag-1 "helm-grep")

(defun helm-x-search-directory (arg)
  (cond ((> (prefix-numeric-value arg) 4)
         (read-directory-name "Directory:"))
        (arg
         default-directory)
        (t
         (let ((project (project-current)))
           (if project
               (project-root project)
             default-directory)))))

(defun helm-x-find (arg)
  (interactive "P")
  (require 'helm-fd)
  (let ((default-directory (helm-x-search-directory arg)))
    (helm-fd-1 default-directory)))

(defun helm-x-grep (arg)
  (interactive "P")
  (require 'helm-grep)
  (let ((default-directory (helm-x-search-directory arg)))
    (helm-grep-ag-1 default-directory)))

(provide 'helm-x)
