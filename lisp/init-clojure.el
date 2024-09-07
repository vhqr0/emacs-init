;;; init-clojure --- Init Clojure -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Clojure language.

;;; Code:

(require 'init-emacs)

(require 'clojure-mode)

(defvar init-clojure-modes '(clojurec-mode clojure-mode clojurescript-mode))
(defvar init-clojure-mode-hooks '(clojurec-mode-hook clojure-mode-hook clojurescript-mode-hook))

(require 'flycheck-clj-kondo)

(dolist (mode init-clojure-modes)
  (add-to-list 'init-evil-eval-function-alist `(,mode . cider-eval-region)))

(require 'cider)

(defun init-lookup-setup-cider ()
  "Setup cider doc."
  (init-lookup-setup-command #'cider-doc))

(dolist (hook '(cider-mode-hook cider-repl-mode-hook))
  (add-hook hook #'init-lookup-setup-cider))

(defun init-counsel-cider-repl-history ()
  "Browse Cider REPL history."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (setq ivy-completion-beg beg)
    (setq ivy-completion-end end)
    (ivy-read "History: " (ivy-history-contents cider-repl-input-history)
              :keymap ivy-reverse-i-search-map
              :initial-input (buffer-substring beg end)
              :action #'counsel--browse-history-action
              :caller #'init-counsel-cider-repl-history)))

(evil-define-key 'insert cider-repl-mode-map
  (kbd "M-r") #'init-counsel-cider-repl-history)

(defvar-keymap init-cider-command-map
  "n" cider-ns-map
  "d" cider-doc-map
  "=" cider-profile-map
  "v" cider-eval-commands-map
  "j" cider-insert-commands-map
  "," cider-test-commands-map)

(init-leader-define-minor-mode-key 'cider-mode
  "y" init-cider-command-map)

(provide 'init-clojure)
;;; init-clojure.el ends here
