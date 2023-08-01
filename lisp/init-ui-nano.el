;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(setq frame-title-format nil
      default-frame-alist
      '((vertical-scroll-bars  . nil)
        (left-fringe           . 0)
        (right-fringe          . 0)
        (internal-border-width . 24)))

(setq window-divider-default-right-width 24)
(window-divider-mode 1)

(defun-add-advice! :after enable-theme init--nanolize (&rest _)
  (interactive)
  (let* ((background         (plist-get (custom-face-attributes-get 'default nil) :background))
         (mode-line          (plist-get (custom-face-attributes-get 'mode-line nil) :background))
         (mode-line-active   (or (plist-get (custom-face-attributes-get 'mode-line-active nil) :background) mode-line))
         (mode-line-inactive (or (plist-get (custom-face-attributes-get 'mode-line-inactive nil) :background) mode-line)))
    (dolist (face '(window-divider window-divider-first-pixel window-divider-last-pixel))
      (set-face-attribute face nil :foreground background))
    (set-face-attribute 'mode-line-active nil :box `(:line-width 6 :color ,mode-line-active :style nil))
    (set-face-attribute 'mode-line-inactive nil :box `(:line-width 6 :color ,mode-line-inactive :style nil))))

(provide 'init-ui-nano)
