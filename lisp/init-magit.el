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

(evil-define-key 'motion magit-section-mode-map
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

(keymap-set magit-mode-map "<remap> <quit-window>" #'magit-mode-bury-buffer)

(evil-set-initial-state 'magit-mode 'normal)

(evil-define-key 'normal magit-mode-map
  "a" #'magit-cherry-apply
  "A" #'magit-cherry-pick
  "b" #'magit-branch
  "B" #'magit-bisect
  "c" #'magit-commit
  "C" #'magit-clone
  "d" #'magit-diff
  "D" #'magit-diff-refresh
  "e" #'magit-ediff-dwim
  "E" #'magit-ediff
  "f" #'magit-fetch
  "F" #'magit-pull
  "i" #'magit-gitignore
  "I" #'magit-init
  "m" #'magit-merge
  "M" #'magit-remote
  "o" #'magit-submodule
  "O" #'magit-subtree
  "p" #'magit-push
  "P" #'magit-push
  "r" #'magit-rebase
  "R" #'magit-file-rename
  "s" #'magit-stage-files
  "S" #'magit-stage-modified
  "t" #'magit-tag
  "T" #'magit-notes
  "u" #'magit-unstage-files
  "U" #'magit-unstage-all
  "w" #'magit-am
  "W" #'magit-patch
  "x" #'magit-reset-quickly
  "X" #'magit-reset
  "z" #'magit-stash
  "Z" #'magit-worktree
  "$" #'magit-process-buffer)

(keymap-set magit-blob-mode-map "<remap> <quit-window>" #'magit-kill-this-buffer)

(evil-define-minor-mode-key 'motion 'magit-blob-mode
  "gj" #'magit-blob-next
  "gk" #'magit-blob-previous
  (kbd "C-j") #'magit-blob-next
  (kbd "C-k") #'magit-blob-previous)

(keymap-set magit-blame-mode-map "<remap> <quit-window>" #'magit-blame-quit)

(evil-define-minor-mode-key 'motion 'magit-blame-mode
  "gj" #'magit-blame-next-chunk
  "gk" #'magit-blame-previous-chunk
  (kbd "C-j") #'magit-blame-next-chunk
  (kbd "C-k") #'magit-blame-previous-chunk)

;;; end

(provide 'init-magit)
;;; init-magit.el ends here
