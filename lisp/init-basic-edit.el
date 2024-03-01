;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)

(setq disabled-command-function nil)

(setq-default
 indent-tabs-mode nil
 truncate-lines t)

(setq word-wrap-by-category t)

(init-global-set-key "C-SPC" #'toggle-input-method)

(defun init-insert-pair-1 (&optional arg open close)
  (interactive "P")
  (insert-pair (or arg '(1)) open close))

(defvar init-sp-bindings
  '(("C-M-f"       . sp-forward-sexp)
    ("C-M-b"       . sp-backward-sexp)
    ("C-M-d"       . sp-down-sexp)
    ("C-M-u"       . sp-backward-up-sexp)
    ("C-M-n"       . sp-next-sexp)
    ("C-M-p"       . sp-previous-sexp)
    ("C-M-k"       . sp-kill-sexp)
    ("C-M-w"       . sp-copy-sexp)
    ("C-M-SPC"     . sp-mark-sexp)
    ("C-k"         . sp-kill-hybrid-sexp)
    ("M-r"         . sp-splice-sexp-killing-around)
    ("M-R"         . sp-splice-sexp-killing-backward)
    ("M-s"         . sp-splice-sexp)
    ("M-S"         . sp-split-sexp)
    ("M-J"         . sp-join-sexp)
    ("M-?"         . sp-convolute-sexp)
    ("C-<right>"   . sp-forward-slurp-sexp)
    ("C-<left>"    . sp-forward-barf-sexp)
    ("C-M-<left>"  . sp-backward-slurp-sexp)
    ("C-M-<right>" . sp-backward-barf-sexp)))

(init-setq-declare!
 sp-ignore-modes-list nil
 sp-base-key-bindings 'paredit
 sp-paredit-bindings init-sp-bindings)

(with-eval-after-load 'smartparens
  (require 'smartparens-config)
  (init-diminish-minor-mode 'smartparens-mode))

(init-eval-after-init!
 (smartparens-global-mode 1)
 (show-smartparens-global-mode 1))

(init-add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(init-setq-declare!
 global-hl-line-sticky-flag t)

(defun init-toggle-line-numbers-type ()
  (interactive)
  (setq-local display-line-numbers-type (if (eq display-line-numbers-type 'relative) t 'relative))
  (display-line-numbers-mode 1))

(init-add-hook '(text-mode-hook prog-mode-hook) #'display-line-numbers-mode)

(init-setq-declare!
 page-break-lines-char ?-)

(init-eval-after-init!
 (global-page-break-lines-mode 1))

(with-eval-after-load 'page-break-lines
  (init-diminish-minor-mode 'page-break-lines-mode))

(provide 'init-basic-edit)
