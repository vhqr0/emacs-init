;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'smartparens))

(require 'init-core-lib)

(init-setq-declare!
 hy-jedhy--enable? nil)

(with-eval-after-load 'hy-mode
  (require 'hy-python))

(init-add-hook 'hy-mode-hook (list #'smartparens-strict-mode #'evil-cleverparens-mode))

(with-eval-after-load 'smartparens
  (sp-with-modes '(hy-mode)
    (sp-local-pair "'" nil :actions nil)))

(defvar page-break-lines-modes)

(with-eval-after-load 'page-break-lines
  (add-to-list 'page-break-lines-modes 'hy-mode))

(defvar evil-x-eval-function-alist)

(with-eval-after-load 'evil-x
  (add-to-list 'evil-x-eval-function-alist '(hy-mode . python-shell-send-region)))

(provide 'init-lang-hy)
