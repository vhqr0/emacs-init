;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)

(init-add-hook
 '(lisp-data-mode-hook emacs-lisp-mode-hook lisp-interaction-mode-hook)
 #'paredit-mode)

(with-eval-after-load 'paredit
  (init-diminish-minor-mode 'paredit-mode))

(init-setq-declare!
 evil-cleverparens-use-regular-insert t
 evil-cleverparens-use-additional-bindings nil
 evil-cleverparens-use-additional-movement-keys nil)

(init-add-hook 'paredit-mode-hook #'evil-cleverparens-mode)

(with-eval-after-load 'evil-cleverparens
  (init-diminish-minor-mode 'evil-cleverparens-mode))

(init-define-key
 '(lisp-data-mode-map emacs-lisp-mode-map lisp-interaction-mode-map)
 "C-c e" #'macrostep-expand)

(provide 'init-lang-lisp)
