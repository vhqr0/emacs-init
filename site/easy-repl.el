(require 'cl-lib)

(defvar easy-repl-backends
  '((emacs-lisp-mode . ielm)
    (lisp-interaction-mode . ielm)
    (sh-mode . shell)
    (python-mode . python)
    (python-ts-mode . python)
    (hy-mode . hy)))

(defvar easy-repl-map
  (let ((map (make-sparse-keymap)))
    (define-key map "o" #'easy-repl-display)
    (define-key map "z" #'easy-repl-switch)
    (define-key map "S" #'easy-repl-shutdown)
    (define-key map "R" #'easy-repl-restart)
    (define-key map "c" #'easy-repl-send-region)
    (define-key map "s" #'easy-repl-send-string)
    map))

(cl-defgeneric easy-repl--get-buffer (backend)
  (user-error "Unimplemented backend"))

(cl-defgeneric easy-repl--get-buffer-create (backend)
  (user-error "Unimplemented backend"))

(cl-defgeneric easy-repl--send-string (backend string)
  (user-error "Unimplemented backend"))

(cl-defgeneric easy-repl--shutdown (backend)
  (let ((buffer (easy-repl--get-buffer backend)))
    (when buffer
      (let ((process (get-buffer-process buffer)))
        (when process
          (kill-process process)
          (while (accept-process-output process)))))))

(cl-defgeneric easy-repl--restart (backend)
  (easy-repl--shutdown backend)
  (easy-repl--get-buffer-create backend))

(cl-defgeneric easy-repl--send-string-and-display (backend string)
  (display-buffer (easy-repl--get-buffer-create backend))
  (easy-repl--send-string backend string))

(defun easy-repl--backend ()
  (cdr (assq major-mode easy-repl-backends)))

(defun easy-repl-display ()
  (interactive)
  (display-buffer (easy-repl--get-buffer-create (easy-repl--backend))))

(defun easy-repl-switch ()
  (interactive)
  (select-window (easy-repl-display)))

(defun easy-repl-shutdown ()
  (interactive)
  (easy-repl--shutdown (easy-repl--backend)))

(defun easy-repl-restart ()
  (interactive)
  (display-buffer (easy-repl--restart (easy-repl--backend))))

(defun easy-repl-send-string (string)
  (interactive (list (read-string "Repl command: ")))
  (easy-repl--send-string-and-display (easy-repl--backend) string))

(defun easy-repl-send-region (start end)
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (easy-repl-send-string (buffer-substring start end)))

;;; ielm

(declare-function inferior-emacs-lisp-mode "ext:ielm")
(declare-function ielm-eval-input "ext:ielm")

(cl-defmethod easy-repl--get-buffer ((backend (eql 'ielm)))
  (get-buffer "*ielm*"))

(cl-defmethod easy-repl--get-buffer-create ((backend (eql 'ielm)))
  (require 'ielm)
  (with-current-buffer (get-buffer-create "*ielm*")
    (unless (comint-check-proc (current-buffer))
      (inferior-emacs-lisp-mode))
    (current-buffer)))

(cl-defmethod easy-repl--send-string ((backend (eql 'ielm)) string)
  (require 'ielm)
  (with-current-buffer (easy-repl--get-buffer-create backend)
    (ielm-eval-input (format "(progn %s)" string))))

;;; shell

(defvar-local sh-shell-process nil)
(declare-function sh-shell-process "ext:sh-script")
(declare-function sh-send-text "ext:sh-script")

(cl-defmethod easy-repl--get-buffer ((backend (eql 'shell)))
  (when sh-shell-process
    (process-buffer sh-shell-process)))

(cl-defmethod easy-repl--get-buffer-create ((backend (eql 'shell)))
  (process-buffer (sh-shell-process t)))

(cl-defmethod easy-repl--send-string ((backend (eql 'shell)) string)
  (sh-send-text string))

(declare-function comint-check-proc "comint")

;;; python

(declare-function python-shell-get-process-name "ext:python")
(declare-function python-shell-calculate-command "ext:python")
(declare-function python-shell-make-comint "ext:python")
(declare-function python-shell-send-string "ext:python")

(cl-defmethod easy-repl--get-buffer ((backend (eql 'python)))
  (require 'python)
  (get-buffer (format "*%s*" (python-shell-get-process-name 'project))))

(cl-defmethod easy-repl--get-buffer-create ((backend (eql 'python)))
  (require 'python)
  (get-buffer (python-shell-make-comint (python-shell-calculate-command) (python-shell-get-process-name 'project))))

(cl-defmethod easy-repl--send-string ((backend (eql 'python)) string)
  (require 'python)
  (with-current-buffer (easy-repl--get-buffer-create backend)
    (python-shell-send-string string (get-process (python-shell-get-process-name 'project)))))

;;; hy

(defvar hy-shell--buffer-name)
(declare-function hy-shell--make-comint "ext:hy-shell")
(declare-function hy-shell--send "ext:hy-shell")

(cl-defmethod easy-repl--get-buffer ((backend (eql 'hy)))
  (require 'hy-shell)
  (get-buffer hy-shell--buffer-name))

(cl-defmethod easy-repl--get-buffer-create ((backend (eql 'hy)))
  (require 'hy-shell)
  (process-buffer (hy-shell--make-comint)))

(cl-defmethod easy-repl--send-string ((backend (eql 'hy)) string)
  (require 'hy-shell)
  (hy-shell--send (format "\n%s\n" string)))

(provide 'easy-repl)
