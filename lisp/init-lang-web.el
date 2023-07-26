;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-basic-prog)

(setq-declare! js
  js-indent-level 2)

(setq-declare! css-mode
  css-indent-offset 2)

(defvar!
 init--prettier-program "prettier"
 init--prettier-option "")

(defun init--prettier-get-command ()
  (format "%s %s --stdin-filepath index.%s"
          init--prettier-program
          init--prettier-option
          (cond ((derived-mode-p 'js-json-mode) "json")
                ((derived-mode-p 'js-mode     ) "js"  )
                ((derived-mode-p 'mhtml-mode  ) "html")
                ((derived-mode-p 'css-mode    ) "css" )
                (t
                 (error (format "init--prettier-option: invalid major mode: %s"
                                (symbol-name major-mode)))))))

(set-format-function! js-json init--prettier-get-command)
(set-format-function! js      init--prettier-get-command)
(set-format-function! mhtml   init--prettier-get-command)
(set-format-function! css     init--prettier-get-command)

(provide 'init-lang-web)
