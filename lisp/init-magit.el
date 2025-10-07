;;; init-magit.el --- Init Magit -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Init configuration to use Magit vc tool.

;;; Code:

(require 'init-emacs)

;;; with editor

(require 'with-editor)

(add-hook 'after-init-hook #'shell-command-with-editor-mode)

(add-hook 'shell-mode-hook #'with-editor-export-editor)
(add-hook 'eshell-mode-hook #'with-editor-export-editor)

;;; magit section

(require 'magit-section)

(evil-set-initial-state 'magit-section-mode 'motion)

(evil-define-key 'motion magit-section-mode-map
  (kbd "TAB") #'magit-section-toggle
  (kbd "S-TAB") #'magit-section-cycle-global
  (kbd "<tab>") #'magit-section-toggle
  (kbd "<backtab>") #'magit-section-cycle-global
  "gj" #'magit-section-forward-sibling
  "gk" #'magit-section-backward-sibling
  (kbd "C-j") #'magit-section-forward-sibling
  (kbd "C-k") #'magit-section-backward-sibling)

;;; magit

(require 'magit)

(defun init-magit-dwim (&optional arg)
  "Call magit dwim.
If in `magit-mode' derived mode, or with more than 2 universal ARG,
or with universal arg and not in a file buffer, call `magit-dispatch';
If with universal arg and in a file buffer, call `magit-file-dispatch';
Or else call `magit-status'."
  (interactive "P")
  (let ((command (cond ((or
                         ;; with more than 2 universal arg
                         (> (prefix-numeric-value arg) 4)
                         ;; with universal arg and not in a file
                         (and arg (not buffer-file-name)))
                        #'magit-dispatch)
                       ;; with universal arg and in a file
                       ((and arg buffer-file-name)
                        #'magit-file-dispatch)
                       (t
                        #'magit-status))))
    (setq this-command command)
    (call-interactively this-command)))

(init-leader-set
 "G" #'init-magit-dwim)

(evil-set-initial-state 'magit-mode 'motion)
(evil-set-initial-state 'magit-status-mode 'motion)
(evil-set-initial-state 'magit-diff-mode 'motion)
(evil-set-initial-state 'magit-log-mode 'motion)
(evil-set-initial-state 'magit-revision-mode 'motion)
(evil-set-initial-state 'magit-stash-mode 'motion)
(evil-set-initial-state 'magit-stashes-mode 'motion)
(evil-set-initial-state 'magit-process-mode 'motion)

(evil-define-key 'motion magit-mode-map
  (kbd "RET") #'magit-visit-thing
  (kbd "<return>") #'magit-visit-thing
  "j" #'magit-next-line
  "k" #'magit-previous-line
  "q" #'magit-mode-bury-buffer
  "gr" #'magit-refresh
  "gR" #'magit-refresh-all
  "p" #'magit-push)

(evil-define-key 'visual magit-mode-map
  "j" #'evil-next-line
  "k" #'evil-previous-line)

(evil-define-minor-mode-key 'motion 'magit-blob-mode
  "gj" #'magit-blob-next
  "gk" #'magit-blob-previous
  (kbd "C-j") #'magit-blob-next
  (kbd "C-k") #'magit-blob-previous
  "q" #'magit-kill-this-buffer)

(evil-define-minor-mode-key 'motion 'magit-blame-mode
  "gj" #'magit-blame-next-chunk
  "gk" #'magit-blame-previous-chunk
  "gJ" #'magit-blame-next-chunk-same-commit
  "gK" #'magit-blame-previous-chunk-same-commit
  (kbd "C-j") #'magit-blame-next-chunk
  (kbd "C-k") #'magit-blame-previous-chunk
  "q" #'magit-blame-quit)

;;; forge

(require 'forge)

(evil-set-initial-state 'forge-topic-mode 'motion)
(evil-set-initial-state 'forge-discussion-mode 'motion)
(evil-set-initial-state 'forge-issue-mode 'motion)
(evil-set-initial-state 'forge-pullreq-mode 'motion)

;;; end

(provide 'init-magit)
;;; init-magit.el ends here
