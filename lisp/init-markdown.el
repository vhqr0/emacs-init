;;; init-markdown.el --- Init Markdown -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Init configuration to use Markdown.

;;; Code:

(require 'init-emacs)
(require 'markdown-mode)
(require 'edit-indirect)

(setq markdown-special-ctrl-a/e t)
(setq markdown-fontify-code-blocks-natively t)

(keymap-set markdown-mode-map "<remap> <init-consult-outline>" #'consult-imenu)

(init-leader-set markdown-mode-map
  "n b" #'markdown-narrow-to-block
  "n s" #'markdown-narrow-to-subtree)

(keymap-set markdown-mode-map "C-c C-'" #'markdown-edit-code-block)
(keymap-set edit-indirect-mode-map "C-c C-'" #'edit-indirect-commit)

(evil-define-key 'motion markdown-mode-map
  (kbd "TAB") #'markdown-cycle
  (kbd "S-TAB") #'markdown-shifttab
  (kbd "<tab>") #'markdown-cycle
  (kbd "<backtab>") #'markdown-shifttab
  "gj" #'markdown-outline-next-same-level
  "gk" #'markdown-outline-previous-same-level
  (kbd "C-j") #'markdown-outline-next-same-level
  (kbd "C-k") #'markdown-outline-previous-same-level)

(provide 'init-markdown)
;;; init-markdown.el ends here
