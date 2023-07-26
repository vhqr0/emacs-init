;;; -*- lexical-binding: t; no-native-compile: t -*-

(defun init--expand-emacs-file-name (x)
  (expand-file-name x user-emacs-directory))

(defun init--expand-init-file-name (x)
  (expand-file-name x init--directory))

(defun init--expand-lisp-file-name (x)
  (expand-file-name x init--lisp-directory))

(defun init--expand-site-file-name (x)
  (expand-file-name x init--site-directory))

(defun init--expand-misc-file-name (x)
  (expand-file-name x init--misc-directory))

(defun init--normalize-symbol-list (x)
  (cond ((symbolp x)
         (list x))
        ((and (listp x) (not (null x)) (symbolp (car x)))
         x)
        (t
         (error (format "init--normalize-symbol-list: invalid type: %s"
                        (symbol-name (type-of x)))))))

(defun init--symbol-add-prefix (prefix x)
  (intern
   (let ((name (symbol-name x)))
     (if (string-prefix-p prefix name)
         name
       (concat prefix name)))))

(defun init--symbol-add-suffix (suffix x)
  (intern
   (let ((name (symbol-name x)))
     (if (string-suffix-p suffix name)
         name
       (concat name suffix)))))

(defun init--normalize-hook-symbol (x)
  (init--symbol-add-suffix "-hook" x))

(defun init--normalize-mode-symbol (x)
  (init--symbol-add-suffix "-mode" x))

(defun init--normalize-map-symbol (x)
  (init--symbol-add-suffix "-map" x))

(defun init--normalize-key (x)
  (cond ((stringp x)
         `(kbd ,x))
        ((integerp x)
         `[,x])
        ((vectorp x)
         x)
        (t
         (error (format "init--normalize-key: invalid type: %s"
                        (symbol-name (type-of x)))))))

(defun init--diminish-minor-mode (x)
  (let ((cons (assq x minor-mode-alist)))
    (unless cons
      (error (format "init--diminish-minor-mode: minor mode not fuound: %s"
                     (symbol-name x))))
    (setcdr cons '(""))))

(provide 'init-core-utils)
