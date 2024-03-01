;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'cl-macs))

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

;;;###autoload
(defun evil-x-jk-setup ()
  (define-key evil-insert-state-map  "j" #'evil-x-jk)
  (define-key evil-replace-state-map "j" #'evil-x-jk))



(evil-define-motion evil-x-jump-item ()
  :jump t
  :type inclusive
  (let* ((thing (sp-get-thing))
         (beg (sp-get thing :beg))
         (end (1- (sp-get thing :end))))
    (goto-char (if (= (point) end) beg end))))

;;;###autoload
(defun evil-x-motion-setup ()
  (define-key evil-motion-state-map "%" #'evil-x-jump-item))



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
    (lisp-interaction-mode . eval-region)))

(evil-define-operator evil-x-operator-eval (beg end)
  :move-point nil
  (interactive "<r>")
  (let ((eval-function (cdr (assq major-mode evil-x-eval-function-alist))))
    (when eval-function
      (funcall eval-function beg end))))

;;;###autoload
(defun evil-x-operator-setup ()
  (define-key evil-normal-state-map "gc" 'evil-x-operator-comment)
  (define-key evil-motion-state-map "g-" 'evil-x-operator-narrow)
  (define-key evil-motion-state-map "gy" 'evil-x-operator-eval))



(evil-define-text-object evil-x-text-object-line (count &optional _beg _end _type)
  (evil-range (line-beginning-position) (line-end-position) 'exclusive))

(evil-define-text-object evil-x-text-object-filename (count &optional _beg _end _type)
  (cl-destructuring-bind (beg . end) (bounds-of-thing-at-point 'filename)
    (evil-range beg end)))

(evil-define-text-object evil-x-text-object-entire (count &optional _beg _end _type)
  (evil-range (point-min) (point-max) 'line))

;;;###autoload
(defun evil-x-text-object-setup ()
  (dolist (binding '(("l" . evil-x-text-object-line)
                     ("u" . evil-x-text-object-filename)
                     ("g" . evil-x-text-object-entire)
                     ("h" . evil-x-text-object-entire)))
    (define-key evil-inner-text-objects-map (car binding) (cdr binding))
    (define-key evil-outer-text-objects-map (car binding) (cdr binding))))



;;;###autoload
(defun evil-x-setup ()
  (evil-x-jk-setup)
  (evil-x-motion-setup)
  (evil-x-operator-setup)
  (evil-x-text-object-setup))

(provide 'evil-x)
