;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-core-utils)

(require 'org)

(setq org-special-ctrl-a/e t
      org-link-descriptive nil)

(add-to-list 'org-modules 'org-tempo)

(define-key! org-mode "<" "\C-q<")

(defun-add-hook! org-mode init--org-fix-angle ()
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(setq-declare! evil-org
  evil-org-key-theme
  '(navigation return textobjects additional shift calendar))

(require 'evil-org)
(diminish! evil-org)
(add-hook! org-mode evil-org-mode)

(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(provide 'init-lang-org-al)
