;;; Package --- Goggles

;;; Commentary:

;; This package provide a minor mode to goggles around editing commands.
;; M-x init-goggles-mode

;;; Code:

(defvar-local init-goggles-changes nil)

(defun init-goggles-pre-command ()
  "Reset change."
  (setq init-goggles-changes nil))

(defun init-goggles-post-command ()
  "Highlight change post command."
  (when init-goggles-changes
    (let ((start most-positive-fixnum)
          (end 0))
      (dolist (change init-goggles-changes)
        (setq start (min start (car change)))
        (setq end (max end (cdr change)))
        (set-marker (car change) nil)
        (set-marker (cdr change) nil))
      (pulse-momentary-highlight-region start end))
    (setq init-goggles-changes nil)))

(defun init-goggles-after-change (start end len)
  "Push change to `init-goggles-changes'.
START END LEN see `after-change-functions'."
  (when (and (/= len 0) (= start end))
    (when (> start (buffer-size))
      (setq start (- start 1)))
    (setq end (1+ start)))
  (let ((change (cons (copy-marker start) (copy-marker end))))
    (push change init-goggles-changes)))

(define-minor-mode init-goggles-mode
  "Init goggles mode."
  :group 'init-goggles
  (if init-goggles-mode
      (progn
        (add-hook 'pre-command-hook #'init-goggles-pre-command nil t)
        (add-hook 'post-command-hook #'init-goggles-post-command nil t)
        (add-hook 'after-change-functions #'init-goggles-after-change nil t))
    (remove-hook 'pre-command-hook #'init-goggles-pre-command t)
    (remove-hook 'post-command-hook #'init-goggles-post-command t)
    (remove-hook 'after-change-functions #'init-goggles-after-change t)))

(add-hook 'prog-mode-hook #'init-goggles-mode)
(add-hook 'text-mode-hook #'init-goggles-mode)
(add-hook 'minibuffer-mode-hook #'init-goggles-mode)

(defun init-evil-around-operator-goggles (func beg end &rest args)
  "Around evil operator do goggles.
FUNC BEG END ARGS see `evil-yank', `evil-delete', etc."
  (when (and init-goggles-mode (called-interactively-p 'interactive))
    (pulse-momentary-highlight-region beg end)
    (sit-for 0.05))
  (apply func beg end args))

(defvar init-evil-goggles-commands
  '(evil-yank
    evil-delete
    evil-change
    evil-indent
    evil-shift-right
    evil-shift-left
    evil-fill-and-move
    evil-surround-region
    evil-Surround-region
    init-evil-operator-comment
    init-evil-operator-narrow
    init-evil-operator-eval))

(dolist (command init-evil-goggles-commands)
  (advice-add command :around #'init-evil-around-operator-goggles))

(provide 'init-goggles)
;;; init-goggles.el ends here
