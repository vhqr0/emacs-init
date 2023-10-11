;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-core-utils)

(setq-declare! helm
  helm-M-x-fuzzy-match nil
  helm-ff-fuzzy-matching nil
  helm-bookmark-show-location t
  helm-buffer-max-length 40
  helm-buffer-skip-remote-checking t)

(declare-function! helm
  helm-resume)

(after-init!
 (global-set-key! "<f5>" #'helm-resume)
 (helm-mode 1)
 (helm-x-setup))

(declare-variable! helm-mode
  helm-completing-read-handlers-alist)

(after-load! helm-mode
  (diminish! helm)
  (add-to-list 'helm-completing-read-handlers-alist '(kill-buffer . nil)))

(defun-add-advice! :after evil-collection-helm-setup
                   init--fix-minibuffer-history-key-after-helm-setup ()
  (custom-set-variables '(helm-minibuffer-history-key "M-r")))

(provide 'init-basic-helm)
