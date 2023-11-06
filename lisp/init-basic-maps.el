;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(defun init--kh-C-u ()
  (interactive)
  (setq prefix-arg
        (list (* 4 (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 1))))
  (set-transient-map evil-x-leader-map))

(defun init--kh-execute-key-binding (key-binding)
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

(defun init--kh-god (prefix)
  (let ((chr (read-char (concat prefix " C-"))))
    (cond ((init--kh-execute-key-binding (key-binding (kbd (format "%s C-%c" prefix chr)))))
          ((init--kh-execute-key-binding (key-binding (kbd (format "%s %c"   prefix chr)))))
          (t
           (error (format "init--kh-god: no key binding found on %s %c" prefix chr))))))

(defun init--kh-C-c ()
  (interactive)
  (init--kh-god "C-c"))

(define-key! evil-x-leader
  "u"         'init--kh-C-u
  "c"         'init--kh-C-c
  "z"         'repeat
  ";"         'eval-expression
  "!"         'shell-command
  "&"         'async-shell-command
  "f"         'helm-find-files
  "b"         'helm-buffers-list
  "j"         'dired-jump
  "k"         'kill-buffer
  "0"         'delete-window
  "1"         'delete-other-windows
  "2"         'split-window-below
  "3"         'split-window-right
  "q"         'quit-window
  "o"         'other-window
  "re"        'helm-recentf
  "rb"        'helm-bookmarks
  "xg"        'revert-buffer-quick
  "xf"        'font-lock-update
  "xn"        'clone-buffer
  "xr"        'rename-buffer
  "ru"        'rename-uniquely
  "x <left>"  'previous-buffer
  "x <right>" 'next-buffer
  "ww"        'evil-window-next
  "wW"        'evil-window-prev
  "wq"        'evil-quit
  "wc"        'evil-window-delete
  "w0"        'evil-window-delete
  "wo"        'delete-other-windows
  "w1"        'delete-other-windows
  "ws"        'evil-window-split
  "wv"        'evil-window-vsplit
  "w2"        'evil-window-split
  "w3"        'evil-window-vsplit
  "w="        'balance-windows
  "wx"        'evil-window-exchange
  "wh"        'evil-window-left
  "wj"        'evil-window-down
  "wk"        'evil-window-up
  "wl"        'evil-window-right
  "wH"        'evil-window-move-far-left
  "wJ"        'evil-window-move-very-bottom
  "wK"        'evil-window-move-very-top
  "wL"        'evil-window-move-far-right
  "w <left>"  'winner-undo
  "w <right>" 'winner-redo
  "44"        'other-window-prefix
  "41"        'same-window-prefix
  "40"        'kill-buffer-and-window
  "4f"        'find-file-other-window
  "4b"        'switch-to-buffer-other-window
  "4j"        'dired-jump-other-window
  "55"        'other-frame-prefix
  "50"        'delete-frame
  "51"        'delete-other-frames
  "52"        'make-frame-command
  "5n"        'clone-frame
  "5u"        'undelete-frame
  "5o"        'other-frame
  "5f"        'find-file-other-frame
  "5b"        'switch-to-buffer-other-frame
  "tt"        'other-tab-prefix
  "t0"        'tab-close
  "t1"        'tab-close-other
  "t2"        'tab-new
  "tn"        'tab-duplicate
  "tu"        'tab-undo
  "to"        'tab-next
  "tO"        'tab-previous
  "tr"        'tab-rename
  "t`"        'toggle-frame-tab-bar
  "tf"        'find-file-other-tab
  "tb"        'switch-to-buffer-other-window
  "pp"        'projectile-switch-project
  "pf"        'helm-projectile-find-file
  "pb"        'helm-projectile-switch-to-buffer
  "pj"        'projectile-dired
  "pi"        'projectile-invalidate-cache
  "px"        'projectile-run-command-in-root
  "p!"        'projectile-run-shell-command-in-root
  "p&"        'projectile-run-async-shell-command-in-root
  "pc"        'projectile-compile-project
  "pv"        'projectile-vc
  "ps"        'projectile-save-project-buffers
  "pk"        'projectile-kill-buffers
  "pg"        'projectile-ripgrep
  "4pf"       'projectile-find-file-other-window
  "4pb"       'projectile-switch-to-buffer-other-window
  "4pj"       'projectile-dired-other-window
  "5pf"       'projectile-find-file-other-frame
  "5pb"       'projectile-switch-to-buffer-other-frame
  "5pj"       'projectile-dired-other-frame
  "hh"        'help-for-help
  "h?"        'help-quick-toggle
  "h."        'display-local-help
  "hi"        'info
  "hl"        'view-lossage
  "he"        'view-echo-area-messages
  "ho"        'helm-apropos
  "hf"        'describe-function
  "hv"        'describe-variable
  "hp"        'describe-package
  "hm"        'describe-mode
  "hb"        'describe-bindings
  "hB"        'describe-keymap
  "hw"        'where-is
  "hk"        'describe-key
  "hc"        'describe-key-briefly
  "htl"       'load-library
  "htf"       'load-file
  "htt"       'init--load-theme
  "hte"       'init--load-theme-random
  "hL"        'find-library
  "hF"        'find-function
  "hV"        'find-variable
  "hK"        'find-function-on-key
  "4hi"       'info-other-window
  "4hL"       'find-library-other-window
  "4hF"       'find-function-other-window
  "4hV"       'find-variable-other-window
  "4hK"       'find-function-on-key-other-window
  "vv"        'magit
  "v?"        'magit-dispatch
  "vf"        'magit-file-dispatch
  "vg"        'vc-update
  "vd"        'vc-diff
  "vD"        'vc-root-diff
  "vl"        'vc-print-log
  "vL"        'vc-print-root-log
  "vh"        'vc-region-history
  "nw"        'widen
  "nn"        'narrow-to-region
  "nd"        'narrow-to-defun
  "np"        'narrow-to-page
  "ss"        'helm-occur
  "sg"        'helm-do-grep-ag
  "si"        'helm-x-imenu
  "sI"        'helm-x-imenu-all
  "s."        'rg-menu
  "sn"        'next-error
  "sp"        'previous-error
  "so"        'occur
  "shu"       'unhighlight-regexp
  "sh."       'highlight-symbol-at-point
  "shr"       'highlight-regexp
  "shp"       'highlight-phrase
  "shl"       'highlight-lines-matching-regexp
  "ma"        'auto-save-visited-mode
  "mt"        'toggle-truncate-lines
  "mh"        'hl-line-mode
  "ml"        'display-line-numbers-mode
  "ms"        'whitespace-mode
  "mv"        'visual-line-mode
  "lu"        'undo-tree-visualize
  "ly"        'helm-show-kill-ring
  "lr"        'helm-register
  "lb"        'ibuffer
  "plb"       'projectile-ibuffer
  "e"         'eshell-dwim
  "pe"        'project-eshell-dwim
  "#"         'server-edit
  "$"         'ispell-word
  "%"         'query-replace-regexp
  "="         'format-all-region-or-buffer
  "."         'xref-find-definitions
  "4."        'xref-find-definitions-other-window
  "5."        'xref-find-definitions-other-frame
  "?"         'xref-find-references
  ","         'xref-go-back
  "("         'init--insert-pair-1
  "["         'init--insert-pair-1
  "{"         'init--insert-pair-1
  "<"         'init--insert-pair-1
  "`"         'init--insert-pair-1
  "'"         'init--insert-pair-1
  "\""        'init--insert-pair-1
  )

(provide 'init-basic-maps)
