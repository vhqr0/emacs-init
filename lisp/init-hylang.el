;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-emacs)

(add-to-list 'sp-lisp-modes 'hy-mode)

(add-to-list 'sp-navigate-skip-match '((hy-mode) . sp--elisp-skip-match))

(let ((entry (assq 'interactive sp-navigate-reindent-after-up)))
  (when (not (memq 'hy-mode (cdr entry)))
    (setcdr entry (cons 'hy-mode (cdr entry)))))

(add-to-list 'sp-sexp-prefix '(hy-mode regexp "\\(?:[@`'#~,_?^]+\\)"))

(sp-local-pair
 'hy-mode "'" nil
 :actions nil)

(sp-local-pair
 'hy-mode "(" nil
 :post-handlers '(:add sp-lisp-insert-space-after-slurp))

(sp-local-pair
 'hy-mode "[" nil
 :post-handlers '(:add sp-lisp-insert-space-after-slurp))

(sp-local-pair
 'hy-mode "`" "`"
 :when '(sp-in-string-p sp-in-comment-p)
 :unless '(sp-lisp-invalid-hyperlink-p))

(provide 'init-hylang)
