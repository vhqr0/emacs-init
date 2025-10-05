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
  "l" #'init-org-roam-node-insert
  "c" #'init-org-roam-node-capture)

(add-to-list 'embark-keymap-alist '(org-roam-node init-org-roam-node-map))

;;;; ref

(defun init-org-roam-ref-url (ref)
  "Get url of an Org Roam REF."
  (let* ((type (get-text-property 0 'type ref))
         (path-end (seq-find
                    (lambda (pos) (get-text-property pos 'invisible ref))
                    (number-sequence 0 (1- (length ref)))))
         (path (substring ref 0 path-end)))
    (concat type ":" path)))

(defun init-org-roam-ref-browse-url (ref)
  "Browse Org Roam REF."
  (browse-url (init-org-roam-ref-url ref)))

(defun init-org-roam-ref-download-url (ref)
  "Download Org Roam REF."
  (embark-download-url (init-org-roam-ref-url ref)))

(defun init-org-roam-ref-open-externally (ref)
  "Open Org Roam REF externally."
  (embark-open-externally (init-org-roam-ref-url ref)))

(defun init-org-roam-ref-eww (ref)
  "Open Org Roam REF with eww."
  (eww (init-org-roam-ref-url ref)))

(defvar-keymap init-org-roam-ref-map
  :parent embark-general-map
  "RET" #'init-org-roam-ref-browse-url
  "b" #'init-org-roam-ref-browse-url
  "d" #'init-org-roam-ref-download-url
  "x" #'init-org-roam-ref-open-externally
  "e" #'init-org-roam-ref-eww)

(add-to-list 'embark-keymap-alist '(org-roam-ref init-org-roam-ref-map))

;;; end

(provide 'init-org-roam)
;;; init-org-roam.el ends here
