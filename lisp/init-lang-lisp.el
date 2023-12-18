;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)

(init-setq-declare!
 evil-cleverparens-use-regular-insert t
 evil-cleverparens-use-additional-bindings nil
 evil-cleverparens-use-additional-movement-keys nil)

(init-add-hook
 '(lisp-data-mode-hook emacs-lisp-mode-hook lisp-interaction-mode-hook)
 (list #'smartparens-strict-mode #'evil-cleverparens-mode))

(with-eval-after-load 'evil-cleverparens
  (init-diminish-minor-mode 'evil-cleverparens-mode))

(init-define-key
 '(emacs-lisp-mode-map lisp-interaction-mode-map)
 [remap display-local-help] #'helpful-at-point
 "C-c e" #'macrostep-expand)

(provide 'init-lang-lisp)
