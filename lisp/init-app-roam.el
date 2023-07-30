;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-core-utils)

(setq-declare! org-roam
  org-roam-directory
  (init--expand-emacs-file-name "notes")
  org-roam-node-display-template
  (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))

(after-load! org
  (org-roam-db-autosync-mode 1))

(provide 'init-app-roam)
