;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'loaddefs-gen)

(message (format "Generating loaddefs %s %s ..."
                 (nth 0 command-line-args-left)
                 (nth 1 command-line-args-left)))
(loaddefs-generate (nth 0 command-line-args-left)
                   (nth 1 command-line-args-left))
