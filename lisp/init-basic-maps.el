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
  "k"     #'kill-buffer
  [left]  #'previous-buffer
  [right] #'next-buffer)

(declare-function! undo-tree
  undo-tree-visualize)

(declare-function! helm
  helm-resume)

(declare-function! helm-x
  helm-x-fd)

(declare-function! org
  org-store-link)

(declare-function! helm-roam
  helm-roam)

(define-key! init--list-prefix
  "b" #'ibuffer
  "u" #'undo-tree-visualize
  "." #'helm-resume
  "s" #'helm-occur
  "g" #'helm-do-grep-ag
  "f" #'helm-x-fd
  "y" #'helm-show-kill-ring
  "r" #'helm-register
  "p" #'helm-browse-project
  "c" #'org-capture
  "a" #'org-agenda
  "w" #'org-store-link
  "n" #'helm-roam)

(setq-declare! key-helper
  key-helper-C-u-transient-map evil-x-leader-map)

(declare-variable! evil
  evil-window-map)

(declare-variable! easy-repl
  easy-repl-map)

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
  ;; FIXME: I think projectile should autoload this map
  ;; "p" projectile-command-map
  "x" easy-repl-map
  "r" ctl-x-r-map
  "4" ctl-x-4-map
  "5" ctl-x-5-map
  "t" tab-prefix-map
  "m" init--minor-prefix-map
  "f" init--file-prefix-map
  "b" init--buffer-prefix-map
  "l" init--list-prefix-map

  ";"   #'eval-expression
  "z"   #'repeat
  "0"   #'delete-window
  "1"   #'delete-other-windows
  "2"   #'split-window-below
  "3"   #'split-window-right
  "o"   #'other-window
  "q"   #'quit-window
  "\""  #'paredit-meta-doublequote
  "("   #'paredit-wrap-round
  "["   #'paredit-wrap-square
  "{"   #'paredit-wrap-curly
  "<"   #'paredit-wrap-angled
  "e"   #'eshell-dwim
  "="   #'format-all-region-or-buffer
  "#"   #'server-edit
  "!"   #'shell-command
  "&"   #'async-shell-command
  "$"   #'ispell-word
  "%"   #'query-replace-regexp
  ","   #'xref-go-back
  "."   #'xref-find-definitions
  "?"   #'xref-find-references)

(provide 'init-basic-maps)
