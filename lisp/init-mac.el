;;; init-mac.el --- Init MacOS -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for MacOS.

;;; Code:

(require 'init-emacs)

(setq mac-command-modifier 'control)

(setenv "GIT_ASKPASS" "git-gui--askpass")
(setenv "SSH_ASKPASS" "git-gui--askpass")

(provide 'init-mac)
;;; init-mac.el ends here
