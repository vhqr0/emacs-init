;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-core-utils)

(setq-declare! evil
  evil-want-keybinding nil
  evil-want-minibuffer t
  evil-want-C-u-delete t
  evil-want-C-u-scroll t
  evil-want-Y-yank-to-eol t
  evil-want-fine-undo t
  evil-undo-system 'undo-tree
  evil-search-module 'evil-search
  evil-ex-search-persistent-highlight nil
  evil-symbol-word-search t
  evil-respect-visual-line-mode t)

(setq-declare! evil-collection
  evil-collection-setup-minibuffer t)

(add-advice! :override evil-set-cursor ignore)

(after-init!
 (evil-mode 1)
 (global-evil-surround-mode 1)
 (evil-collection-init))

(after-load! evil-collection-unimpaired
  (diminish! evil-collection-unimpaired))

(after-load! evil
  (evil-x-setup))

(declare-function! evil
  evil-ex-delete-hl)

;; https://github.com/emacs-evil/evil/pull/1128
(defun-add-advice! :after evil-ex-search
                   init--delete-hl-after-evil-ex-search (&rest _)
  (sit-for 0.3)
  (evil-ex-delete-hl 'evil-ex-search))

(provide 'init-basic-evil)
