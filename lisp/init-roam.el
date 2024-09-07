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
  "l" #'org-roam-buffer-toggle
  "f" #'org-roam-node-find
  "i" #'org-roam-node-insert
  "c" #'org-roam-capture
  "j" #'org-roam-dailies-capture-today)

(global-set-key (kbd "C-c n") init-roam-prefix-map)

(init-leader-global-set-key
 "r n" #'org-roam-node-find)

(provide 'init-roam)
;;; init-roam.el ends here
