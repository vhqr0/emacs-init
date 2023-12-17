;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)

(init-setq-declare!
 org-roam-directory (init-expand-emacs-file-name "notes")
 org-roam-node-display-template (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))

(with-eval-after-load 'org-roam
  (org-roam-db-autosync-mode 1))

(provide 'init-app-roam)
