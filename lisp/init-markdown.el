;;; init-markdown.el --- Init Markdown -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Init configuration to use Markdown.

;;; Code:

(require 'init-emacs)
(require 'markdown-mode)
(require 'edit-indirect)

(setq markdown-special-ctrl-a/e t)
(setq markdown-fontify-code-blocks-natively t)

(add-hook 'markdown-mode-hook #'outline-minor-mode)

(keymap-set markdown-mode-map "<remap> <init-narrow-to-block-placeholder>" #'markdown-narrow-to-block)
(keymap-set markdown-mode-map "<remap> <init-narrow-to-subtree-placeholder>" #'markdown-narrow-to-subtree)
(keymap-set markdown-mode-map "<remap> <init-jump-next-placeholder>" #'markdown-next-visible-heading)
(keymap-set markdown-mode-map "<remap> <init-jump-previous-placeholder>" #'markdown-previous-visible-heading)

(keymap-set markdown-mode-map "C-c C-'" #'markdown-edit-code-block)
(keymap-set edit-indirect-mode-map "C-c C-'" #'edit-indirect-commit)

(provide 'init-markdown)
;;; init-markdown.el ends here
