;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'helm)

(defun helm-x-imenu ()
  (interactive)
  (cond ((derived-mode-p 'comint-mode)
         (helm-comint-prompts))
        ((derived-mode-p 'eshell-mode)
         (helm-eshell-prompts))
        (t
         (helm-imenu))))

(defun helm-x-imenu-all ()
  (interactive)
  (cond ((derived-mode-p 'comint-mode)
         (helm-comint-prompts-all))
        ((derived-mode-p 'eshell-mode)
         (helm-eshell-prompts-all))
        (t
         (helm-imenu-in-all-buffers))))

(declare-function helm-fd-1 "helm-fd")

(defun helm-x-fd (arg)
  (interactive "P")
  (require 'helm-fd)
  (let ((directory
         (if arg
             (file-name-as-directory
              (read-directory-name "DefaultDirectory: "))
           default-directory)))
    (helm-fd-1 directory)))

(defvar helm-x-remap-alist
  '((execute-extended-command               . helm-M-x)
    (yank-pop                               . helm-show-kill-ring)
    (previous-matching-history-element      . helm-minibuffer-history)
    (eshell-previous-matching-input         . helm-eshell-history)
    (comint-history-isearch-backward-regexp . helm-comint-input-ring)
    (dabbrev                                . helm-dabbrev)))

(defvar helm-x-remap-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (remap helm-x-remap-alist)
      (define-key map `[remap ,(car remap)] (cdr remap)))
    map))

(define-minor-mode helm-x-remap-mode
  "Remap normal commands to helm commands."
  :group 'helm
  :global t
  :keymap helm-x-remap-mode-map)

;;;###autoload
(defun helm-x-setup ()
  (helm-x-remap-mode 1))

(provide 'helm-x)
