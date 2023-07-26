;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-basic-prog)

(require 'hy-mode)
(require 'hy-shell)
(require 'evil)

(setq hy-jedhy--enable? nil
      hy-shell--interpreter-args nil)

(add-hook! (hy-mode inferior-hy-mode) paredit-mode)

(evil-define-key 'normal hy-mode-map "gz" #'run-hy)

(define-eval-function! hy
  (hy-shell--eval-1 (buffer-substring beg end)))

(defun init--hy-shell-macroexpand-current-form ()
  (interactive)
  (hy-shell--eval-1
    (format "(hy.macroexpand '%s)" (hy--current-form-string))))

(define-key! hy-mode
  "C-c RET" #'init--hy-shell-macroexpand-current-form)

(provide 'init-lang-hy-al)
