;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-core-utils)

(setq-declare! helm
  helm-bookmark-show-location t
  helm-buffer-max-length 40
  helm-buffer-skip-remote-checking t)

(after-init!
 (helm-mode 1)
 (helm-x-setup))

(declare-variable! helm-mode
  helm-completing-read-handlers-alist)

(after-load! helm-mode
  (diminish! helm)
  (add-to-list 'helm-completing-read-handlers-alist '(kill-buffer . nil)))

(declare-function! evil-collection
  evil-collection-define-key)

(defun-add-advice! :after evil-collection-helm-setup
                   init--evil-helm-custom ()
  (custom-set-variables '(helm-minibuffer-history-key "M-r"))
  (evil-collection-define-key 'normal 'helm-map
    (kbd "SPC") nil
    "m" 'helm-toggle-visible-mark
    "U" 'helm-unmark-all))

(setq-declare! helm
  helm-mini-default-sources '(helm-bufler-source helm-source-recentf))

(after-load! helm-buffers
  (require 'helm-bufler))

(provide 'init-basic-helm)
