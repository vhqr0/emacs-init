;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)

(init-setq-declare!
 org-directory          (init-expand-emacs-file-name "org")
 org-agenda-files       (list org-directory)
 org-default-notes-file (expand-file-name "notes.org" org-directory))

(init-setq-declare!
 org-special-ctrl-a/e t
 org-link-descriptive nil)

(defvar org-modules)
(defvar org-mode-map)
(defvar org-mode-syntax-table)

(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-tempo)
  (init-define-key org-mode-map "<" "\C-q<"))

(defun init-org-fix-angle ()
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(init-add-hook 'org-mode-hook #'init-org-fix-angle)

(init-setq-declare!
 evil-org-key-theme
 '(navigation return textobjects additional calendar))

(init-add-hook 'org-mode-hook #'evil-org-mode)

(with-eval-after-load 'evil-org
  (init-diminish-minor-mode 'evil-org-mode))

(declare-function evil-org-agenda-set-keys "evil-org-agenda")

(with-eval-after-load 'org
  (require 'evil-org-agenda))

(with-eval-after-load 'evil-org-agenda
  (evil-org-agenda-set-keys))

(provide 'init-lang-org)
