;;; init-org-roam.el --- Init Org Roam -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Init configuration to use Org Roam note taking tool.

;;; Code:

(require 'init-org)
(require 'org-roam)

(setq org-roam-directory (expand-file-name "notes" priv-directory))

(setq org-roam-node-display-template
      (concat "${title:*} " (propertize "${tags:30}" 'face 'org-tag)))

(add-hook 'after-init-hook #'org-roam-db-autosync-mode)

(init-leader-global-set
 "N" #'org-roam-node-find
 "R" #'org-roam-ref-find)

(defvar-keymap init-org-roam-command-map
  "n" #'org-roam-node-find
  "l" #'org-roam-node-insert
  "c" #'org-roam-capture
  "b" #'org-roam-buffer-toggle
  "a" #'org-roam-alias-add
  "A" #'org-roam-alias-remove
  "t" #'org-roam-tag-add
  "T" #'org-roam-tag-remove
  "r" #'org-roam-ref-add
  "R" #'org-roam-ref-remove)

(keymap-global-set "C-c n" init-org-roam-command-map)

(defun init-org-roam-node-append ()
  "Append Org Roam node link."
  (interactive)
  (save-excursion
    (unless (eolp)
      (forward-char))
    (call-interactively #'org-roam-node-insert)))

(keymap-set evil-normal-state-map "<remap> <org-roam-node-insert>" #'init-org-roam-node-append)

(defvar-keymap init-org-roam-node-map
  :parent embark-general-map
  "f" #'org-roam-node-find
  "l" #'org-roam-node-insert
  "c" #'org-roam-capture)

(add-to-list 'embark-keymap-alist '(org-roam-node init-org-roam-node-map))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
