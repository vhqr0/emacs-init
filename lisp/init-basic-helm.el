;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)

(declare-function helm-projectile-find-file "helm-projectile")

(init-setq-declare!
 helm-echo-input-in-header-line t
 helm-split-window-default-side 'other
 helm-split-window-other-side-when-one-window 'right
 helm-move-to-line-cycle-in-source nil
 helm-completion-style 'helm-fuzzy
 helm-buffers-fuzzy-matching t
 helm-recentf-fuzzy-match t
 helm-file-cache-fuzzy-match t
 helm-locate-fuzzy-match t
 helm-ls-git-fuzzy-match t
 helm-etags-fuzzy-match t
 helm-apropos-fuzzy-match t
 helm-session-fuzzy-match t
 helm-bookmark-show-location t
 helm-buffer-max-length 40
 helm-buffer-skip-remote-checking t
 helm-grep-file-path-style 'relative
 helm-projectile-truncate-lines t
 projectile-switch-project-action #'helm-projectile-find-file)

(init-eval-after-init!
 (helm-mode 1)
 (require 'helm-projectile))

(init-global-set-key
 "M-x"   #'helm-M-x
 "M-y"   #'helm-show-kill-ring
 "C-c b" #'helm-resume
 "C-c h" #'helm-x-history
 "C-c i" #'helm-x-imenu
 "C-c I" #'helm-x-imenu-all)

(defvar helm-completing-read-handlers-alist)

(with-eval-after-load 'helm-mode
  (init-diminish-minor-mode 'helm-mode)
  (add-to-list 'helm-completing-read-handlers-alist '(kill-buffer . nil)))

(declare-function evil-collection-define-key "evil-collection")

(init-add-advice :override 'helm-minibuffer-history-mode #'ignore)

(defun init-evil-helm-custom ()
  (evil-collection-define-key 'normal 'helm-map
    (kbd "SPC") nil
    "m" 'helm-toggle-visible-mark
    "U" 'helm-unmark-all)
  (evil-collection-define-key '(insert normal) 'helm-map
    (kbd "C-SPC") 'toggle-input-method
    (kbd "C-t") 'helm-toggle-resplit-and-swap-windows))

(init-add-advice :after 'evil-collection-helm-setup #'init-evil-helm-custom)

(provide 'init-basic-helm)
