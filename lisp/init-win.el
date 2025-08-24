;;; init-win.el --- Init Windows -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Windows.

;;; Code:

(require 'init-emacs)

(setenv "GIT_ASKPASS" "git-gui--askpass")
(setenv "SSH_ASKPASS" "git-gui--askpass")

(require 'pcomplete)

(setq pcomplete-hosts-file nil)

(provide 'init-win)
;;; init-win.el ends here
