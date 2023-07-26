;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(setq-declare! org
  org-directory          (init--expand-emacs-file-name "org")
  org-agenda-files       (list org-directory)
  org-default-notes-file (expand-file-name "notes.org" org-directory))

(after-load! org
  (require 'init-lang-org-al))

(provide 'init-lang-org)
