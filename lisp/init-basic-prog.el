;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-core-utils)
(require 'init-basic-emacs)

(comment! projectile
  (setq-declare! projectile
    projectile-switch-project-action #'projectile-commander
    projectile-current-project-on-switch 'move-to-end)

  (after-init!
   (projectile-mode 1))

  (declare-variable! projectile
    projectile-command-map)

  (after-load! projectile
    (define-key! projectile-command
      "x" #'project-execute-extended-command
      "e" #'projectile-run-eshell
      "s" #'projectile-run-shell)))

(comment! xref
  (setq-declare! xref
    xref-search-program 'ripgrep))

(comment! flymake
  (declare-variable! flymake
    flymake-mode-map)

  (declare-function! flymake
    flymake-goto-next-error
    flymake-goto-prev-error)

  (after-load! flymake
    (define-key! flymake-mode
      "M-n" #'flymake-goto-next-error
      "M-p" #'flymake-goto-prev-error))

  (declare-function! flymake-proc
    flymake-proc-legacy-flymake)

  (after-load! flymake-proc
    (remove-hook! flymake-diagnostic-functions flymake-proc-legacy-flymake)))

(comment! eldoc
  (setq eldoc-minor-mode-string nil))

(comment! eglot
  (setq-declare! eglot
    eglot-events-buffer-size 0
    eglot-extend-to-xref t
    eglot-stay-out-of '(company)
    eglot-ignored-server-capabilities
    '(:hoverProvider :documentHighlightProvider)))

(comment! yasnippet
  (setq-declare! yasnippet
    yas-alias-to-yas/prefix-p nil)

  (after-init!
   (yas-global-mode 1))

  (after-load! yasnippet
    (diminish! yas-minor))

  (define-auto-save-visited-predicate! yas
    (and (bound-and-true-p yas-minor-mode)
         (bound-and-true-p yas--active-snippets))))

(provide 'init-basic-prog)
