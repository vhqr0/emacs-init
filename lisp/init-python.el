;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-emacs)

(setq! python-shell-interpreter "ipython")
(setq! python-shell-interpreter-args "--simple-prompt")

(defvar init-python-modes '(python-mode python-ts-mode inferior-python-mode))
(defvar init-python-mode-hooks '(python-base-mode-hook inferior-python-mode-hook))

(defvar elpy-refactor-map)

(with-eval-after-load 'python
  (require 'elpy)
  (elpy-enable)
  (evil-define-key '(motion normal visual operator) elpy-mode-map
    "gr" elpy-refactor-map))

(add-hook 'inferior-python-mode-hook #'python-mls-mode)

(defun init-lookup-setup-elpy () (init-lookup-setup-command #'elpy-doc))

(dolist (hook init-python-mode-hooks)
  (add-hook hook #'init-lookup-setup-elpy))

(dolist (mode init-python-modes)
  (add-to-list 'evil-x-eval-function-alist `(,mode . python-shell-send-region)))

(provide 'init-python)
