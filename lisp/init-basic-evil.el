;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)

(init-setq-declare!
 evil-want-keybinding nil
 evil-want-minibuffer t
 evil-want-C-u-scroll t
 evil-want-Y-yank-to-eol t
 evil-want-fine-undo t
 evil-undo-system 'undo-tree
 evil-search-module 'evil-search
 evil-ex-search-persistent-highlight nil
 evil-symbol-word-search t
 evil-respect-visual-line-mode t)

(init-setq-declare!
 evil-collection-setup-minibuffer t)

(init-add-advice :override 'evil-set-cursor #'ignore)

(init-eval-after-init!
 (evil-mode 1)
 (global-evil-surround-mode 1)
 (evil-collection-init))

(with-eval-after-load 'evil-collection-unimpaired
  (init-diminish-minor-mode 'evil-collection-unimpaired-mode))

(with-eval-after-load 'evil
  (evil-x-setup)
  (init-define-key evil-insert-state-map "C-a" nil "C-k" nil)
  (init-define-key evil-normal-state-map [remap yank-pop] nil))

(declare-function evil-ex-delete-hl "evil")

;; https://github.com/emacs-evil/evil/pull/1128
(defun init-delete-hl-after-evil-ex-search (&rest _)
  (sit-for 0.3)
  (evil-ex-delete-hl 'evil-ex-search))

(init-add-advice :after 'evil-ex-search #'init-delete-hl-after-evil-ex-search)

(provide 'init-basic-evil)
