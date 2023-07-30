;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'helm)
(require 'org-roam)

(defun helm-roam-action-find (candidate)
  (org-roam-node-visit candidate))

(defun helm-roam-action-find-other-window (candidate)
  (org-roam-node-visit candidate t))

(defun helm-roam-action-insert (candidate)
  (insert (org-link-make-string
           (concat "id:" (org-roam-node-id candidate))
           (org-roam-node-formatted candidate))))

(defun helm-roam-action-capture (candidate)
  (org-roam-capture- :node candidate))

(defun helm-roam-dummy-action-create (candidate)
  (org-roam-capture- :node (org-roam-node-create :title candidate)))

(defvar helm-roam-actions
  '(("Find Node"              . helm-roam-action-find)
    ("Find Node Other Window" . helm-roam-action-find-other-window)
    ("Insert Node"            . helm-roam-action-insert)
    ("Capture Node"           . helm-roam-action-capture)))

(defclass helm-roam-class (helm-source-sync)
  ((candidates :initform 'org-roam-node-read--completions)
   (action :initform 'helm-roam-actions)))

(defvar helm-roam-source
  (helm-make-source "Roam Nodes" 'helm-roam-class))

(defvar helm-roam-dummy-source
  (helm-build-dummy-source "Roam Create Node"
    :action (helm-make-actions
             "Create Node"
             #'helm-roam-dummy-action-create)))

;;;###autoload
(defun helm-roam ()
  (interactive)
  (helm :sources '(helm-roam-source helm-roam-dummy-source)
        :buffer "*helm roam*"))

(provide 'helm-roam)
