;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(setq text-quoting-style 'grave)

(after-init!
 (xterm-mouse-mode 1))

(setq-declare! evil
  evil-want-C-i-jump nil)

(declare-function! evil-collection
  evil-collection-define-key)

(defun-add-advice! :after evil-collection-help-setup
                   init--evil-help-revert-C-i-in-term ()
  (evil-collection-define-key 'normal 'help-mode-map
    (kbd "C-i") nil))

(defun-add-advice! :after evil-collection-info-setup
                   init--evil-info-revert-C-i-in-term ()
  (evil-collection-define-key 'normal 'Info-mode-map
    (kbd "C-i") nil))

(global-set-key! "C-M-_" #'dabbrev-completion)

(global-set-key! "C-@" #'toggle-input-method)

(declare-variable! evil
  evil-insert-state-map)

(after-load! evil
  (define-key! evil-insert-state "C-@" #'toggle-input-method))

(setq-declare! magit
  magit-section-visibility-indicator '("..." . t))

(provide 'init-env-term)
