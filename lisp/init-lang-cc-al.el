;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(require 'init-basic-compl)
(require 'init-basic-prog)

(defvar!
 init--clang-format-program "clang-format"
 init--clang-format-option "")

(define-format-function! c
  (format "%s %s" init--clang-format-program init--clang-format-option))

(define-format-function! c++
  (format "%s %s" init--clang-format-program init--clang-format-option))

(defvar!
 init--cc-program "clang"
 init--cc-args nil)

(defun init--flymake-cc-command ()
  `(,init--cc-program "-x" ,(if (derived-mode-p 'c++-mode) "c++" "c")
                      "-fsyntax-only"
                      "-fno-color-diagnostics"
                      "-fno-caret-diagnostics"
                      "-fno-diagnostics-show-option"
                      ,@init--cc-args
                      "-"))

(setq-declare! flymake-cc
  flymake-cc-command #'init--flymake-cc-command)

(defvar!
 init--clangd-program "clangd"
 init--clangd-args "--header-insertion=never")

(declare-variable! eglot
  eglot-server-programs)

(after-load! eglot
  (setq eglot-server-programs
        (cons `((c-mode c++-mode) . (,init--clangd-program ,@init--clangd-args))
              eglot-server-programs)))

(define-company-enabled-mode! cmake
  files cmake)

(provide 'init-lang-cc-al)
