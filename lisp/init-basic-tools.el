;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)
(require 'init-basic-prog)


;;; dired

(init-setq-declare!
 dired-dwim-target t
 dired-listing-switches "-lha")

(with-eval-after-load 'dired
  (put 'dired-jump 'repeat-map nil))


;;; ediff

(declare-function ediff-setup-windows-plain "ediff")

(init-setq-declare!
 ediff-window-setup-function #'ediff-setup-windows-plain)


;;; grep

(init-setq-declare!
 wgrep-auto-save-buffer t
 wgrep-change-readonly-file t)

(autoload 'rg-menu "rg" nil t)


;;; magit

(init-setq-declare!
 magit-bind-magit-project-status nil
 magit-define-global-key-bindings nil)

(init-define-key
 vc-prefix-map
 "j" #'magit-status
 "f" #'magit-file-dispatch
 "?" #'magit-dispatch)


;;; eshell

(defun init-eshell-export-pager ()
  (setenv "PAGER" (init-expand-misc-file-name "pager.py")))

(init-add-hook 'eshell-mode-hook (list #'init-eshell-export-pager #'with-editor-export-editor))

(init-add-advice :override 'evil-collection-eshell-escape-stay #'ignore)

(add-to-list 'init-company-enabled-modes 'eshell-mode)
(add-to-list 'init-company-backends-alist '(eshell-mode . (company-files)))

(defvar eshell-mode-map)

(defun init-eshell-remap-helm-pcomplete ()
  (init-define-key eshell-mode-map [remap completion-at-point] #'helm-esh-pcomplete))

(init-add-hook 'eshell-mode-hook #'init-eshell-remap-helm-pcomplete)


;;; ispell

(init-setq-declare!
 ispell-dictionary "american")

(provide 'init-basic-tools)
