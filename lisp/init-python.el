;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-emacs)

(setq! python-shell-interpreter "ipython")
(setq! python-shell-interpreter-args "--simple-prompt")

(defvar init-python-modes '(python-mode python-ts-mode inferior-python-mode))
(defvar init-python-mode-hooks '(python-base-mode-hook inferior-python-mode-hook))

(add-hook 'inferior-python-mode-hook #'python-mls-mode)

(defun init-python-fix-defaults ()
  (setq-local comment-inline-offset 2
              forward-sexp-function nil))

(dolist (hook init-python-mode-hooks)
  (add-hook hook #'init-python-fix-defaults))

(dolist (mode init-python-modes)
  (add-to-list 'init-evil-eval-function-alist `(,mode . python-shell-send-region)))

;;; smartparens-hy

(add-to-list 'sp-lisp-modes 'hy-mode)

(add-to-list 'sp-navigate-skip-match '((hy-mode) . sp--elisp-skip-match))

(let ((entry (assq 'interactive sp-navigate-reindent-after-up)))
  (when (not (memq 'hy-mode (cdr entry)))
    (setcdr entry (cons 'hy-mode (cdr entry)))))

(add-to-list 'sp-sexp-prefix '(hy-mode regexp "\\(?:[@`'#~,_?^]+\\)"))

(sp-local-pair 'hy-mode "'" nil :actions nil)
(sp-local-pair 'hy-mode "(" nil :post-handlers '(:add sp-lisp-insert-space-after-slurp))
(sp-local-pair 'hy-mode "[" nil :post-handlers '(:add sp-lisp-insert-space-after-slurp))
(sp-local-pair 'hy-mode "`" "`" :when '(sp-in-string-p sp-in-comment-p) :unless '(sp-lisp-invalid-hyperlink-p))

(provide 'init-python)
