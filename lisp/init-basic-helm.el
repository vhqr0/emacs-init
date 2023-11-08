;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-core-utils)

(setq-declare! helm
  helm-bookmark-show-location t
  helm-buffer-max-length 40
  helm-buffer-skip-remote-checking t
  helm-describe-function-function #'helpful-callable
  helm-describe-variable-function #'helpful-variable)

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
    "U" 'helm-unmark-all)
  (evil-collection-define-key '(insert normal) 'helm-map
    (kbd "C-SPC") 'toggle-input-method
    (kbd "C-t") 'helm-toggle-resplit-and-swap-windows))

(provide 'init-basic-helm)
