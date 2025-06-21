;;; init-roam.el --- Init Org Roam -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Init configuration to use Org Roam note taking tool.

;;; Code:

(require 'init-emacs)
(require 'org-roam)

(setq org-roam-directory (expand-file-name "notes" priv-directory))

(setq org-roam-node-display-template
      (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))

(add-hook 'after-init-hook #'org-roam-db-autosync-mode)

(keymap-set init-org-map "n" #'org-roam-node-find)
(keymap-set init-org-map "r" #'org-roam-ref-find)

(defvar-keymap init-org-roam-buffer-mode-map
  "C-c r" #'org-roam-buffer-toggle
  "C-c a t" #'org-roam-tag-add
  "C-c a r" #'org-roam-ref-add
  "C-c a a" #'org-roam-alias-add
  "C-c d t" #'org-roam-tag-remove
  "C-c d r" #'org-roam-ref-remove
  "C-c d a" #'org-roam-alias-remove)

(define-minor-mode init-org-roam-buffer-mode
  "Org Roam buffer minor mode."
  :group 'init-org-roam
  :lighter " Roam"
  :keymap init-org-roam-buffer-mode-map)

(add-hook 'org-roam-find-file-hook #'init-org-roam-buffer-mode)



;;; embark

;;;; node

(defun init-org-roam-node-get (node)
  "Get Org Roam NODE of text."
  (get-text-property 0 'node node))

(defun init-org-roam-node-find (node)
  "Find Org Roam NODE."
  (org-roam-node-visit (init-org-roam-node-get node)))

(defun init-org-roam-node-find-other-window (node)
  "Find Org Roam NODE other window."
  (org-roam-node-visit (init-org-roam-node-get node) t))

(defun init-org-roam-node-insert (node)
  "Insert link of an Org Roam NODE."
  (let ((node (init-org-roam-node-get node)))
    (insert (org-link-make-string
             (concat "id:" (org-roam-node-id node))
             (org-roam-node-formatted node)))))

(defun init-org-roam-node-capture (node)
  "Capture more content to an Org Roam NODE."
  (org-roam-capture- :node (init-org-roam-node-get node)))

(defvar-keymap init-org-roam-node-map
  :parent embark-general-map
  "f" #'init-org-roam-node-find
  "o" #'init-org-roam-node-find-other-window
  "i" #'init-org-roam-node-insert
  "c" #'init-org-roam-node-capture)

(add-to-list 'embark-keymap-alist '(org-roam-node init-org-roam-node-map))

;;;; ref

(defun init-org-roam-ref-url (ref)
  "Get url of an Org Roam REF."
  (let* ((type (get-text-property 0 'type ref))
         (path-end (->> (-iota (length ref))
                        (--first (get-text-property it 'invisible ref))))
         (path (substring ref 0 path-end)))
    (concat type ":" path)))

(defun init-org-roam-ref-browse-default (ref)
  "Browse Org Roam REF."
  (let ((url (init-org-roam-ref-url ref)))
    (message "Browse org roam ref: %s" url)
    (browse-url url)))

(defun init-org-roam-ref-browse-eww (ref)
  "Browse Org Roam REF with eww."
  (let ((url (init-org-roam-ref-url ref)))
    (message "Browse org roam ref: %s" url)
    (eww-browse-url url)))

(defvar-keymap init-org-roam-ref-map
  :parent embark-general-map
  "x" #'init-org-roam-ref-browse-default
  "e" #'init-org-roam-ref-browse-eww)

(add-to-list 'embark-keymap-alist '(org-roam-ref init-org-roam-ref-map))

(provide 'init-roam)
;;; init-roam.el ends here
