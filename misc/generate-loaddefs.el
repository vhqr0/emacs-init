;;; -*- lexical-binding: t; no-native-compile: t -*-

(message (format "Generating loaddefs %s %s ..."
                 (nth 0 command-line-args-left)
                 (nth 1 command-line-args-left)))
(funcall (if (fboundp #'loaddefs-generate)
             #'loaddefs-generate
           #'make-directory-autoloads)
         (nth 0 command-line-args-left)
         (nth 1 command-line-args-left))
