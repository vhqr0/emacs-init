;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)

(init-setq-declare!
 helm-echo-input-in-header-line t
 helm-split-window-default-side 'other
 helm-split-window-other-side-when-one-window 'right
 helm-completion-style 'helm-fuzzy
 helm-buffers-fuzzy-matching t
 helm-recentf-fuzzy-match t
 helm-apropos-fuzzy-match t
 helm-bookmark-show-location t
 helm-buffer-max-length 40
 helm-buffer-skip-remote-checking t
 helm-grep-file-path-style 'relative
 helm-describe-function-function #'helpful-callable
 helm-describe-variable-function #'helpful-variable)

(init-eval-after-init!
 (helm-mode 1))

(init-global-set-key
 "<f5>"  #'helm-resume
 "M-x"   #'helm-M-x
 "M-y"   #'helm-show-kill-ring
 "C-c h" #'helm-x-history)

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
