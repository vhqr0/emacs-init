;;; init-magit.el --- Init Magit -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Init configuration to use Magit vc tool.

;;; Code:

(require 'init-emacs)

;;; with editor

(require 'with-editor)

(shell-command-with-editor-mode 1)

(add-hook 'shell-mode-hook #'with-editor-export-editor)
(add-hook 'eshell-mode-hook #'with-editor-export-editor)

;;; ediff

(require 'ediff)

(setq ediff-window-setup-function #'ediff-setup-windows-plain)

(evil-set-initial-state 'ediff-mode 'emacs)

(defun init-ediff-scroll-up ()
  "Scroll up in ediff."
  (interactive)
  (let ((last-command-event ?V))
    (call-interactively #'ediff-scroll-vertically)))

(defun init-ediff-scroll-down ()
  "Scroll down in ediff."
  (interactive)
  (let ((last-command-event ?v))
    (call-interactively #'ediff-scroll-vertically)))

(defun init-ediff-scroll-left ()
  "Scroll left in ediff."
  (interactive)
  (let ((last-command-event ?>))
    (call-interactively #'ediff-scroll-horizontally)))

(defun init-ediff-scroll-right ()
  "Scroll right in ediff."
  (interactive)
  (let ((last-command-event ?<))
    (call-interactively #'ediff-scroll-horizontally)))

(defun init-ediff-jump-to-last-difference ()
  "Jump to last difference."
  (interactive)
  (ediff-jump-to-difference -1))

(defun init-ediff-setup-keymap-extra ()
  "Setup extra ediff bindings."
  (keymap-set ediff-mode-map "j" #'ediff-next-difference)
  (keymap-set ediff-mode-map "k" #'ediff-previous-difference)
  (keymap-set ediff-mode-map "g g" #'ediff-jump-to-difference)
  (keymap-set ediff-mode-map "G" #'init-ediff-jump-to-last-difference)
  (keymap-set ediff-mode-map "C-d" #'init-ediff-scroll-down)
  (keymap-set ediff-mode-map "C-u" #'init-ediff-scroll-up)
  (keymap-set ediff-mode-map "<up>" #'init-ediff-scroll-up)
  (keymap-set ediff-mode-map "<down>" #'init-ediff-scroll-down)
  (keymap-set ediff-mode-map "<left>" #'init-ediff-scroll-left)
  (keymap-set ediff-mode-map "<right>" #'init-ediff-scroll-right))

(advice-add #'ediff-setup-keymap :after #'init-ediff-setup-keymap-extra)

;;; magit

(require 'magit)
(require 'magit-extras)
(require 'magit-ediff)
(require 'magit-gitignore)
(require 'magit-subtree)
(require 'magit-patch)

(init-leader-set
 "v" #'magit-file-dispatch
 "V" #'magit-dispatch)

(keymap-global-set "<remap> <project-vc-dir>" #'magit-project-status)

(keymap-set magit-mode-map "<remap> <quit-window>" #'magit-mode-bury-buffer)

(evil-set-initial-state 'magit-mode 'normal)

(init-evil-keymap-set 'motion magit-mode-map
  "," #'magit-dispatch)

(init-evil-keymap-set 'normal magit-mode-map
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
(keymap-set magit-blob-mode-map "M-n" #'magit-blob-next)
(keymap-set magit-blob-mode-map "M-p" #'magit-blob-previous)

(keymap-set magit-blame-mode-map "<remap> <quit-window>" #'magit-blame-quit)
(keymap-set magit-blame-mode-map "M-n" #'magit-blame-next-chunk)
(keymap-set magit-blame-mode-map "M-p" #'magit-blame-previous-chunk)

;;; end

(provide 'init-magit)
;;; init-magit.el ends here
