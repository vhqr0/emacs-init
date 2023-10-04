;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(setq-declare! org
  org-directory          (init--expand-emacs-file-name "org")
  org-agenda-files       (list org-directory)
  org-default-notes-file (expand-file-name "notes.org" org-directory))

(setq-declare! org
  org-special-ctrl-a/e t
  org-link-descriptive nil)

(declare-variable! org
  org-modules
  org-mode-map
  org-mode-syntax-table)

(after-load! org
  (add-to-list 'org-modules 'org-tempo)
  (define-key! org-mode "<" "\C-q<"))

(defun-add-hook! org-mode init--org-fix-angle ()
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(setq-declare! evil-org
  evil-org-key-theme
  '(navigation return textobjects additional shift calendar))

(add-hook! org-mode evil-org-mode)

(after-load! evil-org
  (diminish! evil-org))

(declare-function! evil-org-agenda
  evil-org-agenda-set-keys)

(after-load! org
  (require 'evil-org-agenda))

(after-load! evil-org-agenda
  (evil-org-agenda-set-keys))

(provide 'init-lang-org)
