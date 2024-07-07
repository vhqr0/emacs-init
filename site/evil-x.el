;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'cl-lib)
(require 'evil)
(require 'thingatpt)
(require 'smartparens)

(defun evil-x-jk ()
  (interactive)
  (if (or executing-kbd-macro
          defining-kbd-macro
          (sit-for 0.15 'no-redisplay))
      (insert ?j)
    (let ((event (read-event)))
      (if (= event ?k)
          (progn
            (setq this-command 'ignore
                  real-this-command 'ignore)
            (push 'escape unread-command-events))
        (insert ?j)
        (push event unread-command-events)))))

(evil-define-motion evil-x-jump-item ()
  :jump t
  :type inclusive
  (let* ((thing (sp-get-thing))
         (beg (sp-get thing :beg))
         (end (1- (sp-get thing :end))))
    (goto-char (if (= (point) end) beg end))))

(evil-define-operator evil-x-operator-comment (beg end)
  :move-point nil
  (interactive "<r>")
  (comment-or-uncomment-region beg end))

(evil-define-operator evil-x-operator-narrow (beg end)
  :move-point nil
  (interactive "<r>")
  (narrow-to-region beg end))

(defvar evil-x-eval-function-alist
  '((emacs-lisp-mode       . eval-region)
    (lisp-interaction-mode . eval-region)
    (lisp-mode             . lisp-eval-region)
    (python-mode           . python-shell-send-region)
    (python-ts-mode        . python-shell-send-region)))

(evil-define-operator evil-x-operator-eval (beg end)
  :move-point nil
  (interactive "<r>")
  (let ((eval-function (cdr (assq major-mode evil-x-eval-function-alist))))
    (when eval-function
      (funcall eval-function beg end))))

(evil-define-text-object evil-x-inner-line (count &optional _beg _end _type)
  (evil-range (save-excursion
                (goto-char (line-beginning-position))
                (back-to-indentation)
                (point))
              (line-end-position)
              'exclusive))

(evil-define-text-object evil-x-a-line (count &optional _beg _end _type)
  (evil-range (line-beginning-position) (line-end-position) 'line))

(evil-define-text-object evil-x-inner-defun (count &optional beg end _type)
  (evil-select-inner-object 'evil-defun beg end type count t))

(evil-define-text-object evil-x-a-defun (count &optional beg end _type)
  (evil-select-an-object 'evil-defun beg end type count t))

(evil-define-text-object evil-x-inner-comment (count &optional _beg _end _type)
  (cl-destructuring-bind (beg . end) (sp-get-comment-bounds)
    (evil-range (save-excursion
                  (goto-char beg)
                  (forward-word 1)
                  (forward-word -1)
                  (point))
                (save-excursion
                  (goto-char end)
                  (evil-end-of-line)
                  (point))
                'block)))

(evil-define-text-object evil-x-a-comment (count &optional _beg _end _type)
  (cl-destructuring-bind (beg . end) (sp-get-comment-bounds)
    (evil-range beg end 'exclusive)))

(evil-define-text-object evil-x-text-object-url (count &optional _beg _end _type)
  (cl-destructuring-bind (beg . end) (bounds-of-thing-at-point 'filename)
    (evil-range beg end 'inclusive)))

(evil-define-text-object evil-x-text-object-entire (count &optional _beg _end _type)
  (evil-range (point-min) (point-max) 'line))

(defun evil-x-default-keybindings ()
  ;; escape
  (define-key evil-insert-state-map  "j" #'evil-x-jk)
  (define-key evil-replace-state-map "j" #'evil-x-jk)
  ;; motions
  (define-key evil-motion-state-map "%" #'evil-x-jump-item)
  ;; operations
  (define-key evil-normal-state-map "gc" 'evil-x-operator-comment)
  (define-key evil-motion-state-map "g-" 'evil-x-operator-narrow)
  (define-key evil-motion-state-map "gy" 'evil-x-operator-eval)
  ;; text objects
  (define-key evil-inner-text-objects-map "l" #'evil-x-inner-line)
  (define-key evil-outer-text-objects-map "l" #'evil-x-a-line)
  (define-key evil-inner-text-objects-map "d" #'evil-x-inner-defun)
  (define-key evil-outer-text-objects-map "d" #'evil-x-a-defun)
  (define-key evil-inner-text-objects-map "c" #'evil-x-inner-comment)
  (define-key evil-outer-text-objects-map "c" #'evil-x-a-comment)
  (define-key evil-inner-text-objects-map "u" #'evil-x-text-object-url)
  (define-key evil-outer-text-objects-map "u" #'evil-x-text-object-url)
  (define-key evil-inner-text-objects-map "h" #'evil-x-text-object-entire)
  (define-key evil-outer-text-objects-map "h" #'evil-x-text-object-entire))

(provide 'evil-x)
