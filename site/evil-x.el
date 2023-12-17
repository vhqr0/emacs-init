;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'cl-macs))

(require 'evil)
(require 'thingatpt)

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



(defvar evil-x-C-p-last-commands
  '(evil-paste-after evil-paste-before evil-past-pop evil-paste-pop-next))
(defvar evil-x-C-p-continue-command #'evil-paste-pop)
(defvar evil-x-C-p-command #'helm-mini)

(defun evil-x-C-p-filter (_)
  (if (memq last-command evil-x-C-p-last-commands)
      evil-x-C-p-continue-command
    evil-x-C-p-command))

(defvar evil-x-C-p-menu-item
  `(menu-item "" nil :filter evil-x-C-p-filter))



(defun evil-x-shift-menu-item (x)
  `(menu-item "" nil :filter (lambda (_) (key-binding (vector ,x)))))

;; ?\, and ?\. are exclusive
;; ?^ is special for ?\^ is not a char
(defconst evil-x-shift-special-chars
  '((?1  . ?\!) (?2  . ?\@) (?3  . ?\#) (?4  . ?\$) (?5  . ?\%)
    (?6  . ?^ ) (?7  . ?\&) (?8  . ?\*) (?9  . ?\() (?0  . ?\))
    (?\` . ?\~) (?\- . ?\_) (?\= . ?\+) (?\[ . ?\{) (?\] . ?\})
    (?\\ . ?\|) (?\; . ?\:) (?\' . ?\") (?\/ . ?\?)))

(defvar evil-x-shift-map
  (let ((map (make-sparse-keymap)))
    (cl-loop for chr from ?a to ?z
             do (define-key map (vector chr) (evil-x-shift-menu-item (upcase chr))))
    (cl-loop for cons in evil-x-shift-special-chars
             do (define-key map (vector (car cons)) (evil-x-shift-menu-item (cdr cons))))
    map))

;;;###autoload
(defvar evil-x-leader-map (make-sparse-keymap))



(defvar evil-x-override-mode-map (make-sparse-keymap))

(define-minor-mode evil-x-override-mode
  "Override global and local evil maps."
  :group 'evil
  :global t
  :keymap evil-x-override-mode-map)

(evil-define-key '(motion normal visual operator) evil-x-override-mode-map
  ","    evil-x-shift-map
  "\s"   evil-x-leader-map
  "\C-p" evil-x-C-p-menu-item)



;;;###autoload
(defun evil-x-setup ()
  (evil-x-jk-setup)
  (evil-x-operator-setup)
  (evil-x-text-object-setup)
  (evil-x-override-mode 1))

(provide 'evil-x)
