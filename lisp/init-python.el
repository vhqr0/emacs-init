;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-emacs)

(setq! python-shell-interpreter "ipython")
(setq! python-shell-interpreter-args "--simple-prompt")

(setq! elpy-modules
       '(elpy-module-company
         elpy-module-eldoc
         elpy-module-flymake
         elpy-module-django))

(setq! elpy-formatter 'black)
(setq! elpy-get-info-from-shell t)
(setq! elpy-remove-modeline-lighter nil)

(defvar init-python-modes '(python-mode python-ts-mode inferior-python-mode))
(defvar init-python-mode-hooks '(python-base-mode-hook inferior-python-mode-hook))

(defvar elpy-mode-map)
(defvar elpy-refactor-map)

(declare-function elpy-doc "elpy")
(declare-function elpy-format-code "elpy")

(with-eval-after-load 'python
  (require 'elpy)
  (elpy-enable)
  (evil-define-key '(motion normal visual operator) elpy-mode-map
    "gr" elpy-refactor-map)
  (define-key elpy-mode-map [remap format-all-region-or-buffer] #'elpy-format-code))

(add-hook 'inferior-python-mode-hook #'python-mls-mode)

(defun init-python-fix-defaults ()
  (setq-local comment-inline-offset 2
              forward-sexp-function nil))

(defun init-lookup-setup-elpy () (init-lookup-setup-command #'elpy-doc))

(dolist (hook init-python-mode-hooks)
  (add-hook hook #'init-python-fix-defaults)
  (add-hook hook #'init-lookup-setup-elpy))

(dolist (mode init-python-modes)
  (add-to-list 'evil-x-eval-function-alist `(,mode . python-shell-send-region)))

(provide 'init-python)
