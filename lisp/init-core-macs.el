;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'cl-macs))

(require 'init-core-utils)

(defmacro defvar! (&rest clauses)
  `(progn
     ,@ (cl-loop while clauses
                 collect `(defvar ,(pop clauses) ,(pop clauses)))))

(defmacro override-variable! (file &rest clauses)
  (declare (indent 1))
  (ignore file)
  `(defvar! ,@clauses))

(defmacro declare-variable! (file &rest vars)
  (declare (indent 1))
  (ignore file)
  `(progn
     ,@ (cl-loop for var in vars collect `(defvar ,var))))

(defmacro declare-function! (file &rest functions)
  (declare (indent 1))
  (let ((file (if (symbolp file) (symbol-name file) file)))
    `(progn
       ,@ (cl-loop for function in functions
                   collect `(declare-function ,function ,file)))))

(defmacro setq-declare! (file &rest clauses)
  (declare (indent 1))
  (ignore file)
  `(progn
     ,@ (cl-loop while clauses
                 collect (let ((name (pop clauses)))
                           `(progn
                              (defvar ,name)
                              (setq ,name ,(pop clauses)))))))

(defmacro init--map-hook-function! (hook-fn hooks functions)
  (let ((hooks (mapcar #'init--normalize-hook-symbol
                       (init--normalize-symbol-list hooks)))
        (functions (init--normalize-symbol-list functions)))
    `(progn
       ,@ (cl-loop for hook in hooks
                   nconc (cl-loop for function in functions
                                  collect `(,hook-fn ',hook #',function))))))

(defmacro add-hook! (hooks functions)
  `(init--map-hook-function! add-hook ,hooks ,functions))

(defmacro remove-hook! (hooks functions)
  `(init--map-hook-function! remove-hook ,hooks ,functions))

(defmacro defun-add-hook! (hook name arglist &rest body)
  (declare (indent 3))
  `(progn
     (defun ,name ,arglist
       ,@body)
     (add-hook! ,hook ,name)))

(defmacro add-advice! (how symbols functions)
  (let ((symbols (init--normalize-symbol-list symbols))
        (functions (init--normalize-symbol-list functions)))
    `(progn
       ,@ (cl-loop for symbol in symbols
                   nconc (cl-loop for function in functions
                                  collect `(advice-add ',symbol ,how #',function))))))

(defmacro remove-advice! (symbols functions)
  (let ((symbols (init--normalize-symbol-list symbols))
        (functions (init--normalize-symbol-list functions)))
    `(progn
       ,@ (cl-loop for symbol in symbols
                   nconc (cl-loop for function in functions
                                  collect `(advice-remove ',symbol #',function))))))

(defmacro defun-add-advice! (how symbol name arglist &rest body)
  (declare (indent 4))
  `(progn
     (defun ,name ,arglist
       ,@body)
     (add-advice! ,how ,symbol ,name)))

(defmacro after-load! (package &rest body)
  (declare (indent 1))
  `(with-eval-after-load ',package
     ,@body))

(defmacro after-init! (&rest body)
  `(add-hook 'after-init-hook (lambda () ,@body)))

(defmacro init--autoload! (functions file &rest args)
  (let ((file (if (symbolp file) (symbol-name file) file))
        (functions (init--normalize-symbol-list functions)))
    `(progn
       ,@ (cl-loop for function in functions
                   collect `(autoload ',function ,file ,@args)))))

(defmacro autoload-function! (file &rest functions)
  (declare (indent 1))
  `(init--autoload! ,functions ,file))

(defmacro autoload-command! (file &rest commands)
  (declare (indent 1))
  `(init--autoload! ,commands ,file nil t))

(defmacro diminish! (modes)
  (let ((modes (mapcar #'init--normalize-mode-symbol
                       (init--normalize-symbol-list modes))))
    `(progn
       ,@ (cl-loop for mode in modes
                   collect `(init--diminish-minor-mode ',mode)))))

(defmacro global-set-key! (&rest bindings)
  `(progn
     ,@ (cl-loop while bindings
                 collect `(global-set-key ,(init--normalize-key (pop bindings)) ,(pop bindings)))))

(defmacro define-key! (maps &rest bindings)
  (declare (indent 1))
  (let ((maps (mapcar #'init--normalize-map-symbol
                      (init--normalize-symbol-list maps)))
        (bindings (cl-loop while bindings
                           collect `(,(init--normalize-key (pop bindings)) . ,(pop bindings)))))
    `(progn
       ,@ (cl-loop for map in maps
                   nconc (cl-loop for binding in bindings
                                  collect `(define-key ,map ,(car binding) ,(cdr binding)))))))

(provide 'init-core-macs)
