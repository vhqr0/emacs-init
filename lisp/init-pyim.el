;;; init-pyim.el --- Init PY Input Method -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Init configuration to use PY Input Method.

;;; Code:

(require 'init-emacs)
(require 'posframe)
(require 'pyim)
(require 'pyim-basedict)

(setq default-input-method "pyim")

(defvar init-pyim-zirjma-keymaps
  '(("a"    "a"    "a"          )
    ("b"    "b"    "ou"         )
    ("c"    "c"    "iao"        )
    ("d"    "d"    "uang" "iang")
    ("e"    "e"    "e"          )
    ("f"    "f"    "en"         )
    ("g"    "g"    "eng"        )
    ("h"    "h"    "ang"        )
    ("i"    "ch"   "i"          )
    ("j"    "j"    "an"         )
    ("k"    "k"    "ao"         )
    ("l"    "l"    "ai"         )
    ("m"    "m"    "ian"        )
    ("n"    "n"    "in"         )
    ("o"    "o"    "uo"   "o"   )
    ("p"    "p"    "un"         )
    ("q"    "q"    "iu"         )
    ("r"    "r"    "uan"  "er"  )
    ("s"    "s"    "iong" "ong" )
    ("t"    "t"    "ue"   "ve"  )
    ("u"    "sh"   "u"          )
    ("v"    "zh"   "v"    "ui"  )
    ("w"    "w"    "ia"   "ua"  )
    ("x"    "x"    "ie"         )
    ("y"    "y"    "uai"  "ing" )
    ("z"    "z"    "ei"         )
    ("aa"   "a"                 )
    ("ah"   "ang"               )
    ("ai"   "ai"                )
    ("aj"   "an"                )
    ("ak"   "ao"                )
    ("al"   "ai"                )
    ("an"   "an"                )
    ("ao"   "ao"                )
    ("ee"   "e"                 )
    ("ef"   "en"                )
    ("eg"   "eng"               )
    ("ei"   "ei"                )
    ("en"   "en"                )
    ("er"   "er"                )
    ("ez"   "ei"                )
    ("ob"   "ou"                )
    ("oo"   "o"                 )
    ("ou"   "ou"                )))

(setq pyim-default-scheme 'zirjma)
(setq pyim-pinyin-fuzzy-alist nil)
(setq pyim-enable-shortcode nil)
(setq pyim-candidates-search-buffer-p nil)
(setq pyim-indicator-list nil)
(setq pyim-page-tooltip '(posframe))

(setq pyim-punctuation-dict
      '(("'"  "‘"  "’")
        ("\"" "“"  "”")
        ("^"  "…"     )
        ("$"  "¥"     )
        ("("  "（"    )
        (")"  "）"    )
        ("["  "【"    )
        ("]"  "】"    )
        ("{"  "「"    )
        ("}"  "」"    )
        ("<"  "《"    )
        (">"  "》"    )
        ("?"  "？"    )
        ("!"  "！"    )
        (","  "，"    )
        ("."  "。"    )
        (";"  "；"    )
        (":"  "："    )
        ("\\" "、"    )))

(defun init-pyim-probe-evil ()
  "Disable input method in evil non-insertion modes."
  (and (not isearch-mode)
       evil-local-mode
       (memq evil-state '(operator motion normal visual))))

(setq pyim-english-input-switch-functions (list #'init-pyim-probe-evil))

(pyim-scheme-add
 `(zirjma
   :document "zirjma"
   :class shuangpin
   :first-chars "abcdefghijklmnopqrstuvwxyz"
   :rest-chars "abcdefghijklmnopqrstuvwxyz"
   :prefer-triggers nil
   :cregexp-support-p t
   :keymaps ,init-pyim-zirjma-keymaps))

(keymap-set pyim-mode-map "." #'pyim-page-next-page)
(keymap-set pyim-mode-map "," #'pyim-page-previous-page)

(add-hook 'after-init-hook #'pyim-basedict-enable)

(defun init-pyim-after-self-insert-command-check-escape ()
  "Check jk escape in pyim."
  (when (pyim-process-with-entered-buffer
          (equal "jk" (buffer-substring (point-min) (point-max))))
    (push 'escape unread-command-events)))

(advice-add #'pyim-self-insert-command :after #'init-pyim-after-self-insert-command-check-escape)

(defun init-pyim-around-self-insert-command (func)
  "Disable `self-insert-command' like command when use pyim.
FUNC see `init-evil-escape'."
  (if (and (equal current-input-method "pyim")
           (not (pyim-process-auto-switch-english-input-p)))
      (progn
        (setq this-command #'self-insert-command)
        (setq real-this-command #'self-insert-command)
        (call-interactively #'self-insert-command))
    (funcall func)))

(advice-add #'init-evil-escape :around #'init-pyim-around-self-insert-command)

(provide 'init-pyim)
;;; init-pyim.el ends here
