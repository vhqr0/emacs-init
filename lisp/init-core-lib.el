;;; -*- lexical-binding: t; no-native-compile: t -*-

(defmacro init-eval-after-init! (&rest body)
  `(add-hook 'after-init-hook (lambda () ,@ body)))

(defmacro init-setq-declare! (sym var &rest clauses)
  `(progn
     (defvar ,sym)
     (setq ,sym ,var)
     ,@ (when clauses
          `((init-setq-declare! ,@ clauses)))))



(defun init-funcall-product2 (f x y)
  (cond ((listp x) (dolist (i x) (init-funcall-product2 f i y)))
        ((listp y) (dolist (i y) (init-funcall-product2 f x i)))
        (t (funcall f x y))))

(defun init-add-hook (hook function)
  (init-funcall-product2 #'add-hook hook function))

(defun init-remove-hook (hook function)
  (init-funcall-product2 #'remove-hook hook function))

(defun init-add-advice (how symbol function)
  (init-funcall-product2 #'(lambda (s f) (advice-add s how f)) symbol function))

(defun init-remove-advice (symbol function)
  (init-funcall-product2 #'advice-remove symbol function))

(defun init-append-to-list (list-var elements)
  (dolist (element elements)
    (add-to-list list-var element)))



(defun init-kbd (key)
  (if (stringp key) (kbd key) key))

(defun init-keymap (keymap)
  (if (symbolp keymap) (symbol-value keymap) keymap))

(defun init-global-set-key (key def &rest clauses)
  (global-set-key (init-kbd key) def)
  (when clauses
    (apply #'init-global-set-key clauses)))

(defun init-define-key (keymap key def &rest clauses)
  (let ((key (init-kbd key)))
    (if (keymapp keymap)
        (define-key (init-keymap keymap) key def)
      (dolist (m keymap)
        (define-key (init-keymap m) key def))))
  (when clauses
    (apply #'init-define-key keymap clauses)))



(defun init-diminish-minor-mode (mode)
  (if (symbolp mode)
      (setq minor-mode-alist
            (cl-remove-if #'(lambda (cons) (eq (car cons) mode))
                          minor-mode-alist))
    (dolist (m mode)
      (init-diminish-minor-mode m))))



(defun init-expand-emacs-file-name (x)
  (expand-file-name x user-emacs-directory))

(defun init-expand-lisp-file-name (x)
  (expand-file-name x init-lisp-directory))

(defun init-expand-site-file-name (x)
  (expand-file-name x init-site-directory))

(defun init-expand-misc-file-name (x)
  (expand-file-name x init-misc-directory))

(provide 'init-core-lib)
