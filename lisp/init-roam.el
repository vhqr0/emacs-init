;;; init-roam --- Init Org Roam -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Init configuration to use Org Roam note taking tool.

;;; Code:

(require 'init-emacs)

(setq! org-roam-directory (expand-file-name "notes" priv-directory))

(setq! org-roam-node-display-template
       (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))

(require 'org-roam)

(org-roam-db-autosync-mode 1)

(keymap-set init-app-map "n" #'org-roam-node-find)
(keymap-set init-app-map "r" #'org-roam-ref-find)

(keymap-set org-mode-map "C-c r" #'org-roam-buffer-toggle)
(keymap-set org-mode-map "C-c a t" #'org-roam-tag-add)
(keymap-set org-mode-map "C-c a a" #'org-roam-alias-add)
(keymap-set org-mode-map "C-c a r" #'org-roam-ref-add)
(keymap-set org-mode-map "C-c d t" #'org-roam-tag-remove)
(keymap-set org-mode-map "C-c d a" #'org-roam-alias-remove)
(keymap-set org-mode-map "C-c d r" #'org-roam-ref-remove)

(provide 'init-roam)
;;; init-roam.el ends here
