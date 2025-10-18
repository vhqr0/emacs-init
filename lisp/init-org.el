;;; init-org.el --- Init Org Mode -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Init configuration to use Org Mode.

;;; Code:

(require 'init-emacs)

;;; outline

(require 'outline)

(setq outline-minor-mode-cycle t)
(setq outline-minor-mode-highlight 'override)
(setq outline-minor-mode-use-buttons 'in-margins)

(keymap-set outline-mode-map "<remap> <init-jump-next-placeholder>" #'outline-next-visible-heading)
(keymap-set outline-mode-map "<remap> <init-jump-previous-placeholder>" #'outline-previous-visible-heading)
(keymap-set outline-minor-mode-map "<remap> <init-jump-next-placeholder>" #'outline-next-visible-heading)
(keymap-set outline-minor-mode-map "<remap> <init-jump-previous-placeholder>" #'outline-previous-visible-heading)

;;; org

(require 'org)
(require 'org-macs)
(require 'org-agenda)
(require 'org-capture)
(require 'embark-org)

(add-to-list 'org-modules 'org-tempo)

(setq org-special-ctrl-a/e t)
(setq org-sort-function #'org-sort-function-fallback)
(setq org-tags-sort-function #'org-string<)

(setq org-directory (expand-file-name "org" user-emacs-directory))
(setq org-agenda-files (list org-directory))
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

(setq org-capture-templates
      '(("t" "Todo"                      entry (file "") "* TODO %?\n%U")
        ("a" "Todo With Annotation"      entry (file "") "* TODO %?\n%U\n%a")
        ("i" "Todo With Initial Content" entry (file "") "* TODO %?\n%U\n%i")
        ("c" "Todo With Kill Ring"       entry (file "") "* TODO %?\n%U\n%c")))

(defun init-org-set-syntax ()
  "Modify `org-mode' syntax table."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(add-hook 'org-mode-hook #'init-org-set-syntax)

(keymap-set org-mode-map "<remap> <init-consult-outline>" #'consult-imenu)
(keymap-set org-mode-map "<remap> <init-narrow-to-block-placeholder>" #'org-narrow-to-block)
(keymap-set org-mode-map "<remap> <init-narrow-to-subtree-placeholder>" #'org-narrow-to-subtree)
(keymap-set org-mode-map "<remap> <init-jump-next-placeholder>" #'org-next-visible-heading)
(keymap-set org-mode-map "<remap> <init-jump-previous-placeholder>" #'org-previous-visible-heading)

(keymap-set org-mode-map "C-c C-'" #'org-edit-special)
(keymap-set org-src-mode-map "C-c C-'" #'org-edit-src-exit)
(keymap-set org-src-mode-map "C-c C-c" #'org-edit-src-exit)

(init-leader-set
 "A" #'org-agenda
 "C" #'org-capture
 "W" #'org-store-link
 "O" #'org-open-at-point-global
 "L" #'org-insert-link-global)

(keymap-global-set "C-c o" #'org-open-at-point-global)
(keymap-global-set "C-c l" #'org-insert-link-global)

(keymap-set org-mode-map "<remap> <org-open-at-point-global>" #'org-open-at-point)
(keymap-set org-mode-map "<remap> <org-insert-link-global>" #'org-insert-link)

(defun init-org-append-link ()
  "Append org link."
  (interactive)
  (save-excursion
    (unless (eolp)
      (forward-char))
    (call-interactively #'org-insert-link)))

(defun init-org-append-link-global ()
  "Append org link outside org."
  (interactive)
  (save-excursion
    (unless (eolp)
      (forward-char))
    (call-interactively #'org-insert-link-global)))

(keymap-set evil-normal-state-map "<remap> <org-insert-link>" #'init-org-append-link)
(keymap-set evil-normal-state-map "<remap> <org-insert-link-global>" #'init-org-append-link-global)

(defun init-org-echo-link ()
  "Echo org link in minibuffer."
  (interactive)
  (when (org-in-regexp org-link-any-re)
    (let (message-log-max)
      (message "%s" (match-string-no-properties 0)))))

(keymap-set embark-org-link-map "e" #'init-org-echo-link)

;;; end

(provide 'init-org)
;;; init-org.el ends here
