;;; outline-x.el --- Outline extensions -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: outlines

;;; Commentary:

;; Outline extensions:
;;
;; - `outline-x-narrow-to-subtree' narrows to the subtree at point.
;; - `outline-x-lisp-setup' sets ;;; style comment headings, levels by
;;   the number of semicolons, and enables `outline-minor-mode'.
;; - `outline-x-comint-setup' sets the prompts of `comint-prompt-regexp'
;;   as headings.
;; - `outline-x-eshell-setup' sets the eshell prompts as headings.
;;
;;   (keymap-set narrow-map "s" #'outline-x-narrow-to-subtree)
;;   (add-hook 'emacs-lisp-mode-hook #'outline-x-lisp-setup)
;;   (add-hook 'comint-mode-hook #'outline-x-comint-setup)
;;   (add-hook 'eshell-mode-hook #'outline-x-eshell-setup)

;;; Code:

(require 'outline)

;;;###autoload
(defun outline-x-narrow-to-subtree ()
  "Narrow to outline subtree."
  (interactive)
  (save-excursion
    (save-match-data
      (narrow-to-region
       (progn (outline-back-to-heading t) (point))
       (progn (outline-end-of-subtree)
              (when (and (outline-on-heading-p) (not (eobp)))
                (backward-char 1))
              (point))))))

;;; lisp

(defun outline-x-lisp-level ()
  "Return level of current outline heading."
  (when (looking-at ";;\\([;*]+\\)")
    (- (match-end 1) (match-beginning 1))))

;;;###autoload
(defun outline-x-lisp-setup ()
  "Set outline vars for Lisp, and enable `outline-minor-mode'."
  (setq-local outline-regexp ";;[;*]+[\s\t]+")
  (setq-local outline-level #'outline-x-lisp-level)
  (outline-minor-mode 1))

;;; comint

(defvar comint-prompt-regexp)

;;;###autoload
(defun outline-x-comint-setup ()
  "Set outline vars for comint."
  (setq-local outline-regexp comint-prompt-regexp)
  (setq-local outline-level (lambda () 1)))

;;; eshell

;;;###autoload
(defun outline-x-eshell-setup ()
  "Set outline vars for Eshell."
  (setq-local outline-regexp "^[^#$\n]* [#$] ")
  (setq-local outline-level (lambda () 1)))

(provide 'outline-x)
;;; outline-x.el ends here
