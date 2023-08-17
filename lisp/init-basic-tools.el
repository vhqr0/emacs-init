;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-core-utils)
(require 'init-basic-compl)

(comment! dired
  (setq-declare! dired
    dired-dwim-target t
    dired-listing-switches "-lha")
  (put 'dired-jump 'repeat-map nil))

(comment! ediff
  (declare-function! ediff
    ediff-setup-windows-plain)

  (setq-declare! ediff
    ediff-window-setup-function #'ediff-setup-windows-plain))

(comment! magit
  (setq-declare! magit
    magit-bind-magit-project-status nil
    magit-define-global-key-bindings nil)

  (define-key! vc-prefix-map
    "j" 'magit-status
    "f" 'magit-file-dispatch
    "?" 'magit-dispatch))

(comment! eshell
  (declare-variable! eshell
    eshell-mode-map)

  (defun-add-hook! eshell-mode
      init--eshell-export-pager ()
    (setenv "PAGER" (init--expand-misc-file-name "pager.py")))
  (add-hook! eshell-mode with-editor-export-editor)

  (add-advice! :override evil-collection-eshell-escape-stay ignore)

  (define-company-enabled-mode! eshell
    files)

  (defun-add-hook! eshell-mode
      init--eshell-remap-helm-pcomplete ()
    (define-key! eshell-mode [remap completion-at-point] #'helm-esh-pcomplete)))

(comment! ispell
  (setq-declare! ispell
    ispell-dictionary "american"))

(comment! x-utils
  (x-utils-setup))

(provide 'init-basic-tools)
