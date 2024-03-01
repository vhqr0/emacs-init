;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)

(init-setq-declare!
 evil-want-keybinding nil
 evil-want-minibuffer t
 evil-want-C-u-scroll t
 evil-want-Y-yank-to-eol t
 evil-want-fine-undo t
 evil-undo-system 'undo-tree
 evil-symbol-word-search t
 evil-respect-visual-line-mode t
 evil-snipe-repeat-keys nil)

(init-setq-declare!
 evil-collection-setup-minibuffer t)

(init-eval-after-init!
 (evil-mode 1)
 (global-evil-surround-mode 1)
 (evil-snipe-mode 1)
 (evil-snipe-override-mode 1)
 (require 'evil-multiedit)
 (evil-multiedit-default-keybinds)
 (evil-collection-init))

(with-eval-after-load 'evil-snipe
  (init-diminish-minor-mode 'evil-snipe-local-mode))

(with-eval-after-load 'evil-collection-unimpaired
  (init-diminish-minor-mode 'evil-collection-unimpaired-mode))

(init-add-advice :override 'evil-set-cursor #'ignore)

(with-eval-after-load 'evil
  (evil-x-setup)
  (init-define-key evil-insert-state-map "C-a" nil "C-@" nil "C-k" nil "C-w" nil)
  (init-define-key evil-normal-state-map [remap yank-pop] nil))

(defvar init-evil-disable-adjust-cursor-commands
  '(sp-forward-sexp sp-previous-sexp forward-sexp forward-list))

(defun init-evil-disable-adjust-cursor (func &rest args)
  (unless (memq this-command init-evil-disable-adjust-cursor-commands)
    (apply func args)))

(init-add-advice :around 'evil-adjust-cursor #'init-evil-disable-adjust-cursor)

(provide 'init-basic-evil)
