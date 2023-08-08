;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'cl-macs))

(require 'evil)
(require 'thingatpt)

(defvar evil-x-escape-fkey ?j)
(defvar evil-x-escape-skey ?k)

(defvar evil-x-escape-delay 0.15)

(defvar evil-x-escape-maps '(evil-insert-state-map evil-replace-state-map))

(defvar evil-x-escape-ignore nil)

(defun evil-x-escape-filter (_)
  (cond ((or executing-kbd-macro defining-kbd-macro)
         nil)
        (evil-x-escape-ignore
         (setq evil-x-escape-ignore nil)
         nil)
        ((sit-for evil-x-escape-delay 'no-redisplay)
         (push evil-x-escape-fkey unread-command-events)
         (setq evil-x-escape-ignore t)
         'ignore)
        (t
         (let ((evt (read-event)))
           (if (eq evt evil-x-escape-skey)
               (progn
                 (evil-repeat-stop)
                 (push 'escape unread-command-events))
             (push evt unread-command-events)
             (push evil-x-escape-fkey unread-command-events)
             (setq evil-x-escape-ignore t)))
         'ignore)))

(defvar evil-x-escape-menu-item
  '(menu-item "" nil :filter evil-x-escape-filter))

;;;###autoload
(defun evil-x-escape-setup ()
  (dolist (map evil-x-escape-maps)
    (define-key (symbol-value map) (vector evil-x-escape-fkey) evil-x-escape-menu-item)))

(defvar evil-x-C-p-menu-item
  `(menu-item "" nil :filter evil-x-C-p-filter))

(evil-define-operator evil-x-operator-comment (beg end)
  :move-point nil
  (interactive "<r>")
  (comment-or-uncomment-region beg end))

(evil-define-operator evil-x-operator-narrow (beg end)
  :move-point nil
  (interactive "<r>")
  (narrow-to-region beg end))

;;;###autoload
(defun evil-x-operator-setup ()
  (define-key evil-normal-state-map "gc" 'evil-x-operator-comment)
  (define-key evil-motion-state-map "g-" 'evil-x-operator-narrow))

(evil-define-text-object evil-x-text-object-line (count &optional beg end type)
  (evil-range (line-beginning-position) (line-end-position) 'exclusive))

(evil-define-text-object evil-x-text-object-filename (count &optional beg end type)
  (cl-destructuring-bind (beg . end) (bounds-of-thing-at-point 'filename)
    (evil-range beg end)))

(evil-define-text-object evil-x-text-object-defun (count &optional beg end type)
  (cl-destructuring-bind (beg . end) (bounds-of-thing-at-point 'defun)
    (evil-range beg end 'line)))

(evil-define-text-object evil-x-text-object-entire (count &optional beg end type)
  (evil-range (point-min) (point-max) 'line))

;;;###autoload
(defun evil-x-text-object-setup ()
  (dolist (binding '(("l" . evil-x-text-object-line)
                     ("d" . evil-x-text-object-defun)
                     ("f" . evil-x-text-object-defun)
                     ("u" . evil-x-text-object-filename)
                     ("F" . evil-x-text-object-filename)
                     ("g" . evil-x-text-object-entire)
                     ("h" . evil-x-text-object-entire)))
    (define-key evil-inner-text-objects-map (car binding) (cdr binding))
    (define-key evil-outer-text-objects-map (car binding) (cdr binding))))

(defvar evil-x-window-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "w" #'evil-window-next)
    (define-key map "W" #'evil-window-prev)
    map))

(declare-function winner-undo "winner")
(declare-function winner-redo "winner")

;;;###autoload
(defun evil-x-window-setup ()
  (define-key evil-window-map "0"     #'evil-window-delete)
  (define-key evil-window-map "1"     #'delete-other-windows)
  (define-key evil-window-map "2"     #'evil-window-split)
  (define-key evil-window-map "3"     #'evil-window-vsplit)
  (define-key evil-window-map [left]  #'winner-undo)
  (define-key evil-window-map [right] #'winner-redo)
  (put #'evil-window-next 'repeat-map 'evil-x-window-repeat-map)
  (put #'evil-window-prev 'repeat-map 'evil-x-window-repeat-map))

(defvar evil-x-C-p-last-commands
  '(evil-paste-after evil-paste-before evil-past-pop evil-paste-pop-next))
(defvar evil-x-C-p-continue-command #'evil-paste-pop)
(defvar evil-x-C-p-command #'helm-mini)

(defun evil-x-C-p-filter (_)
  (if (memq last-command evil-x-C-p-last-commands)
      evil-x-C-p-continue-command
    evil-x-C-p-command))

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
  (evil-x-escape-setup)
  (evil-x-operator-setup)
  (evil-x-text-object-setup)
  (evil-x-window-setup)
  (evil-x-override-mode 1))

(provide 'evil-x)
