;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-core-utils)

(comment! paredit
  (add-hook! (lisp-data-mode emacs-lisp-mode lisp-interaction-mode) paredit-mode)
  (after-load! paredit
    (diminish! paredit))
  (setq-declare! evil-cleverparens
    evil-cleverparens-use-regular-insert t
    evil-cleverparens-use-additional-bindings nil
    evil-cleverparens-use-additional-movement-keys nil)
  (add-hook! paredit-mode evil-cleverparens-mode)
  (after-load! evil-cleverparens
    (diminish! evil-cleverparens)))

(comment! macrostep
  (define-key! (lisp-data-mode emacs-lisp-mode lisp-interaction-mode)
    "C-c e" #'macrostep-expand))

(provide 'init-lang-lisp)
