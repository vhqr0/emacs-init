;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)

(setq inhibit-startup-screen t
      initial-scratch-message nil)

(setq use-dialog-box nil
      use-file-dialog nil)

(setq ring-bell-function #'ignore)



(defvar init-ui-disabled-modes
  '(blink-cursor-mode tooltip-mode tool-bar-mode menu-bar-mode scroll-bar-mode))

(dolist (mode init-ui-disabled-modes)
  (when (fboundp mode)
    (funcall mode -1)))



(defvar init-load-theme-after-init t)

(defun init-load-theme (theme)
  (interactive
   (list (intern (completing-read "Load custom theme: "
                                  (mapcar #'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (message "load theme: %s" theme))

(defun init-load-theme-random ()
  (interactive)
  (init-load-theme (seq-random-elt (custom-available-themes))))

(when init-load-theme-after-init
  (init-eval-after-init!
   (init-load-theme-random)))



(setq tab-bar-tab-hints t
      tab-bar-select-tab-modifiers '(meta))

(init-global-set-key
 "C-S-T" #'tab-bar-new-tab
 "C-S-W" #'tab-bar-close-tab
 "C--"   #'text-scale-decrease
 "C-="   #'text-scale-increase)

(provide 'init-basic-ui)
