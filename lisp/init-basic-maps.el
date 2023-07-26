;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(define-key! help
  "t"  nil
  "tt" #'load-theme
  "tf" #'load-file
  "tl" #'load-library
  "j"  #'find-library
  "4j" #'find-library-other-window
  "5j" #'find-library-other-frame)

(define-key! ctl-x-r
  "e" #'recentf-open)

(define-key! ctl-x-4
  "j" #'dired-jump-other-window)

(defvar!
 init--minor-prefix-map  (make-sparse-keymap)
 init--file-prefix-map   (make-sparse-keymap)
 init--buffer-prefix-map (make-sparse-keymap)
 init--list-prefix-map   (make-sparse-keymap))

(define-key! init--minor-prefix
  "a" #'auto-save-visited-mode
  "t" #'toggle-truncate-lines
  "h" #'hl-line-mode
  "l" #'display-line-numbers-mode
  "s" #'whitespace-mode
  "v" #'visual-line-mode)

(define-key! init--file-prefix
  "f" #'find-file
  "r" #'find-file-read-only
  "v" #'find-alternate-file
  "d" #'dired
  "j" #'dired-jump
  "o" #'x-utils-xdg-open
  "i" #'insert-file
  "w" #'write-file)

(define-key! init--buffer-prefix
  "f"     #'font-lock-update
  "g"     #'revert-buffer-quick
  "r"     #'rename-buffer
  "u"     #'rename-uniquely
  "n"     #'clone-buffer
  "i"     #'insert-buffer
  "q"     #'bury-buffer
  "Q"     #'unbury-buffer
  "b"     #'switch-to-buffer
  "s"     #'save-buffer
  "k"     #'kill-buffer-dwim
  [left]  #'previous-buffer
  [right] #'next-buffer)

(declare-function! undo-tree
  undo-tree-visualize)

(declare-function! org
  org-store-link)

(define-key! init--list-prefix
  "b" #'ibuffer
  "u" #'undo-tree-visualize
  "c" #'org-capture
  "a" #'org-agenda
  "w" #'org-store-link
  "g" #'helm-do-grep-ag
  "f" #'helm-multi-files
  "p" #'helm-browse-project)

(setq-declare! key-helper
  key-helper-C-u-transient-map evil-x-leader-map)

(declare-variable! evil
  evil-window-map)

(declare-variable! projectile
  projectile-command-map)

(declare-function! evil
  evil-ex)

(declare-function! paredit
  paredit-meta-doublequote
  paredit-wrap-round
  paredit-wrap-square
  paredit-wrap-curly
  paredit-wrap-angled)

(declare-function! server
  server-edit)

(define-key! evil-x-leader
  "u" #'key-helper-C-u
  "c" #'key-helper-C-c

  "w" evil-window-map
  "h" help-map
  "g" goto-map
  "s" search-map
  "n" narrow-map
  "v" vc-prefix-map
  "p" projectile-command-map
  "r" ctl-x-r-map
  "4" ctl-x-4-map
  "5" ctl-x-5-map
  "t" tab-prefix-map
  "m" init--minor-prefix-map
  "f" init--file-prefix-map
  "b" init--buffer-prefix-map
  "l" init--list-prefix-map

  "SPC" #'evil-ex
  "x"   #'execute-extended-command
  ";"   #'eval-expression
  "z"   #'repeat
  "\""  #'paredit-meta-doublequote
  "("   #'paredit-wrap-round
  "["   #'paredit-wrap-square
  "{"   #'paredit-wrap-curly
  "<"   #'paredit-wrap-angled
  "e"   #'eshell-dwim
  "="   #'prog-x-format-dwim
  "#"   #'server-edit
  "!"   #'shell-command
  "&"   #'async-shell-command
  "$"   #'ispell-word
  "%"   #'query-replace-regexp
  ","   #'xref-go-back
  "."   #'xref-find-definitions
  "?"   #'xref-find-references)

(provide 'init-basic-maps)
