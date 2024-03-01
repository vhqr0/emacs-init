;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)

(defun init-kh-C-u ()
  (interactive)
  (setq prefix-arg
        (list (* 4 (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 1))))
  (set-transient-map evil-x-leader-map))

(defun init-kh-execute-key-binding (key-binding)
  (cond ((commandp key-binding)
         (setq this-command key-binding
               real-this-command key-binding)
         (if (commandp key-binding t)
             (call-interactively key-binding)
           (execute-kbd-macro key-binding))
         key-binding)
        ((keymapp key-binding)
         (set-transient-map key-binding)
         key-binding)))

(defun init-kh-god (prefix)
  (let ((char (read-char (concat prefix " C-"))))
    (cond ((init-kh-execute-key-binding (key-binding (kbd (format "%s C-%c" prefix char)))))
          ((init-kh-execute-key-binding (key-binding (kbd (format "%s %c"   prefix char)))))
          (t
           (error (format "init-kh-god: no key binding found on %s %c" prefix char))))))

(defun init-kh-C-c ()
  (interactive)
  (init-kh-god "C-c"))

(defun init-define-leader (&rest clauses)
  (apply #'init-define-key evil-x-leader-map clauses))



;;; embark
(init-global-set-key "M-o" #'embark-act)

(init-eval-after-init!
 (repeat-mode 1))

(declare-function undo-tree-visualize "undo-tree")
(declare-function server-edit "server")

;;; basic
(init-define-leader
 "z"  #'repeat
 "u"  #'init-kh-C-u
 "c"  #'init-kh-C-c
 ";"  #'eval-expression
 "!"  #'shell-command
 "&"  #'async-shell-command
 "#"  #'server-edit
 "e"  #'eshell-dwim
 "pe" #'project-eshell-dwim
 "U"  #'undo-tree-visualize)

(init-eval-after-init!
 (winner-mode 1))

(declare-function winner-undo "winner")
(declare-function winner-redo "winner")

;;; window
(init-define-leader
 "0"         #'delete-window
 "1"         #'delete-other-windows
 "2"         #'split-window-below
 "3"         #'split-window-right
 "q"         #'quit-window
 "o"         #'other-window
 "ww"        #'evil-window-next
 "wW"        #'evil-window-prev
 "wq"        #'evil-quit
 "wc"        #'evil-window-delete
 "w0"        #'evil-window-delete
 "wo"        #'delete-other-windows
 "w1"        #'delete-other-windows
 "ws"        #'evil-window-split
 "wv"        #'evil-window-vsplit
 "w2"        #'evil-window-split
 "w3"        #'evil-window-vsplit
 "w="        #'balance-windows
 "wx"        #'evil-window-exchange
 "wh"        #'evil-window-left
 "wj"        #'evil-window-down
 "wk"        #'evil-window-up
 "wl"        #'evil-window-right
 "wH"        #'evil-window-move-far-left
 "wJ"        #'evil-window-move-very-bottom
 "wK"        #'evil-window-move-very-top
 "wL"        #'evil-window-move-far-right
 "w <left>"  #'winner-undo
 "w <right>" #'winner-redo)

;;; buffer
(init-define-leader
 "f"         #'helm-find-files
 "b"         #'helm-buffers-list
 "j"         #'dired-jump
 "k"         #'kill-buffer
 "l"         #'ibuffer
 "rm"        #'bookmark-set
 "rb"        #'helm-bookmarks
 "re"        #'helm-recentf
 "rc"        #'org-capture
 "ro"        #'org-agenda
 "rn"        #'helm-roam
 "xg"        #'revert-buffer-quick
 "xf"        #'font-lock-update
 "xn"        #'clone-buffer
 "xr"        #'rename-buffer
 "xu"        #'rename-uniquely
 "xo"        #'xdg-open
 "x <left>"  #'previous-buffer
 "x <right>" #'next-buffer)

;;; other-window/frame/tab
(init-define-leader
 "44" #'other-window-prefix
 "41" #'same-window-prefix
 "40" #'kill-buffer-and-window
 "4f" #'find-file-other-window
 "4b" #'switch-to-buffer-other-window
 "4j" #'dired-jump-other-window
 "55" #'other-frame-prefix
 "50" #'delete-frame
 "51" #'delete-other-frames
 "52" #'make-frame-command
 "5n" #'clone-frame
 "5u" #'undelete-frame
 "5o" #'other-frame
 "5f" #'find-file-other-frame
 "5b" #'switch-to-buffer-other-frame
 "tt" #'other-tab-prefix
 "t0" #'tab-close
 "t1" #'tab-close-other
 "t2" #'tab-new
 "tn" #'tab-duplicate
 "tu" #'tab-undo
 "to" #'tab-next
 "tO" #'tab-previous
 "tr" #'tab-rename
 "t`" #'toggle-frame-tab-bar
 "tf" #'find-file-other-tab
 "tb" #'switch-to-buffer-other-window)

;;; project
(init-define-leader
 "pp"  #'projectile-switch-project
 "pf"  #'helm-projectile-find-file
 "pd"  #'helm-projectile-find-dir
 "pb"  #'helm-projectile-switch-to-buffer
 "pj"  #'projectile-dired
 "pl"  #'projectile-ibuffer
 "pi"  #'projectile-invalidate-cache
 "px"  #'projectile-run-command-in-root
 "p!"  #'projectile-run-shell-command-in-root
 "p&"  #'projectile-run-async-shell-command-in-root
 "pc"  #'projectile-compile-project
 "pv"  #'projectile-vc
 "ps"  #'projectile-save-project-buffers
 "pk"  #'projectile-kill-buffers
 "pg"  #'projectile-ripgrep
 "4pf" #'projectile-find-file-other-window
 "4pb" #'projectile-switch-to-buffer-other-window
 "4pj" #'projectile-dired-other-window
 "5pf" #'projectile-find-file-other-frame
 "5pb" #'projectile-switch-to-buffer-other-frame
 "5pj" #'projectile-dired-other-frame)

;;; vc
(init-define-leader
 "vv" #'magit
 "v?" #'magit-dispatch
 "vf" #'magit-file-dispatch
 "vg" #'vc-refresh-state
 "vd" #'vc-diff
 "vD" #'vc-root-diff
 "vl" #'vc-print-log
 "vL" #'vc-print-root-log
 "vh" #'vc-region-history)

(autoload 'rg-menu "rg" nil t)
(declare-function rg-menu "rg")
(declare-function rg-save-search "rg")
(declare-function rg-save-search-as-name "rg")

;;; search
(init-define-leader
 "s"  #'helm-occur
 "S"  #'helm-x-grep
 "F"  #'helm-x-find
 "i"  #'helm-x-imenu
 "I"  #'helm-x-imenu-all
 "go" #'occur
 "gg" #'rg-menu
 "gd" #'rg-dwim
 "gc" #'rg-dwim-current-dir
 "gf" #'rg-dwim-current-file
 "gr" #'rg
 "gt" #'rg-literal
 "gl" #'rg-list-searches
 "gs" #'rg-save-search
 "gS" #'rg-save-search-as-name
 "gn" #'next-error
 "gp" #'previous-error
 "nw" #'widen
 "nn" #'narrow-to-region
 "nd" #'narrow-to-defun
 "np" #'narrow-to-page)

;;; help
(init-define-leader
 "hh"  #'help-for-help
 "h?"  #'help-quick-toggle
 "h."  #'display-local-help
 "hi"  #'info
 "hl"  #'view-lossage
 "he"  #'view-echo-area-messages
 "ho"  #'helm-apropos
 "hx"  #'helpful-command
 "hf"  #'helpful-function
 "hv"  #'helpful-variable
 "hp"  #'describe-package
 "hm"  #'describe-mode
 "hb"  #'describe-bindings
 "hB"  #'describe-keymap
 "hw"  #'where-is
 "hk"  #'helpful-key
 "hc"  #'describe-key-briefly
 "htl" #'load-library
 "htf" #'load-file
 "htt" #'init-load-theme
 "hte" #'init-load-theme-random
 "hL"  #'find-library
 "hF"  #'find-function
 "hV"  #'find-variable
 "hK"  #'find-function-on-key
 "4hi" #'info-other-window
 "4hL" #'find-library-other-window
 "4hF" #'find-function-other-window
 "4hV" #'find-variable-other-window
 "4hK" #'find-function-on-key-other-window)

;;; minor-mode
(init-define-leader
 "ma" #'auto-save-visited-mode
 "mt" #'toggle-truncate-lines
 "mh" #'hl-line-mode
 "ml" #'display-line-numbers-mode
 "mL" #'init-toggle-line-numbers-type
 "ms" #'whitespace-mode
 "mv" #'visual-line-mode)

;;; prog
(init-define-leader
 "$"  #'ispell-word
 "%"  #'query-replace-regexp
 "="  #'format-all-region-or-buffer
 "."  #'xref-find-definitions
 "4." #'xref-find-definitions-other-window
 "5." #'xref-find-definitions-other-frame
 "?"  #'xref-find-references
 ","  #'xref-go-back
 "("  #'init-insert-pair-1
 "["  #'init-insert-pair-1
 "{"  #'init-insert-pair-1
 "<"  #'init-insert-pair-1
 "`"  #'init-insert-pair-1
 "'"  #'init-insert-pair-1
 "\"" #'init-insert-pair-1)

(provide 'init-basic-maps)
