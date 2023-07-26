;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(setq-default
 indent-tabs-mode nil
 truncate-lines t)

(setq word-wrap-by-category t)

(comment! paren
  (setq-declare! paren
    show-paren-context-when-offscreen t)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (add-hook! prog-mode rainbow-delimiters-mode))

(comment! line
  (setq-declare! display-line-numbers
    display-line-numbers-type 'relative)
  (setq-declare! hl-line
    global-hl-line-sticky-flag t)
  (add-hook! (text-mode prog-mode) display-line-numbers-mode))

(comment! isearch
  (setq isearch-lazy-count t
        isearch-allow-scroll t
        isearch-allow-motion t
        isearch-yank-on-move t
        isearch-motion-changes-direction t
        isearch-repeat-on-direction-change t)
  (define-key! isearch-mode
    "<f2>" #'isearch-occur
    "M-."  #'isearch-forward-symbol-at-point))

(provide 'init-basic-edit)
