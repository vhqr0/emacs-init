;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)

(setq text-quoting-style 'grave)

(init-eval-after-init!
 (xterm-mouse-mode 1))

(init-setq-declare!
 evil-want-C-i-jump nil)

(declare-function evil-collection-define-key "evil-collection")

(defun init-evil-help-revert-C-i-in-term ()
  (evil-collection-define-key 'normal 'help-mode-map (kbd "C-i") nil))
(init-add-advice :after 'evil-collection-help-setup #'init-evil-help-revert-C-i-in-term)

(defun init-evil-info-revert-C-i-in-term ()
  (evil-collection-define-key 'normal 'Info-mode-map (kbd "C-i") nil))
(init-add-advice :after 'evil-collection-info-setup #'init-evil-info-revert-C-i-in-term)

(init-global-set-key "C-M-_" #'dabbrev-completion)

(init-global-set-key "C-@" #'toggle-input-method)

(defvar evil-insert-state-map)

(with-eval-after-load 'evil
  (init-define-key evil-insert-state-map "C-@" #'toggle-input-method))

(init-setq-declare!
 magit-section-visibility-indicator '("..." . t))

(provide 'init-env-term)
