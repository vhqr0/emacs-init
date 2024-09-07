;;; init-roam --- Init Org Roam -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Init configuration to use Org Roam note taking tool.

;;; Code:

(require 'init-emacs)

(setq! org-roam-directory (expand-file-name "notes" user-emacs-directory))

(setq! org-roam-node-display-template
       (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))

(require 'org-roam)

(org-roam-db-autosync-mode 1)

(defvar-keymap init-roam-prefix-map
  "f" #'org-roam-node-find
  "c" #'org-roam-capture
  "i" #'org-roam-node-insert
  "l" #'org-roam-buffer-toggle
  "g" #'org-roam-graph
  "j" #'org-roam-dailies-capture-today)

(global-set-key (kbd "C-c n") init-roam-prefix-map)

(init-leader-global-set-key
 "n f" #'org-roam-node-find
 "n c" #'org-roam-capture)

(provide 'init-roam)
;;; init-roam.el ends here
