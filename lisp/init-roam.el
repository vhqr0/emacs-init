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

(global-set-key (kbd "C-c n") #'helm-roam)

(init-leader-global-set-key
 "r n" #'helm-roam)

(init-leader-define-key org-mode-map
  "y r" #'org-roam-buffer-toggle
  "y a a" #'org-roam-alias-add
  "y a t" #'org-roam-tag-add
  "y a r" #'org-roam-ref-add
  "y d a" #'org-roam-alias-remove
  "y d t" #'org-roam-tag-remove
  "y d r" #'org-roam-ref-remove)

(provide 'init-roam)
;;; init-roam.el ends here
