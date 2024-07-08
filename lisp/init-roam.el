;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-emacs)

(setq! org-roam-directory (expand-file-name "notes" user-emacs-directory))

(require 'org-roam)

(org-roam-db-autosync-mode 1)

(defvar-keymap init-roam-prefix-map
  "l" #'org-roam-buffer-toggle
  "f" #'org-roam-node-find
  "g" #'org-roam-graph
  "i" #'org-roam-node-insert
  "c" #'org-roam-capture
  "j" #'org-roam-dailies-capture-today)

(global-set-key (kbd "C-c n") init-roam-prefix-map)

(provide 'init-roam)
