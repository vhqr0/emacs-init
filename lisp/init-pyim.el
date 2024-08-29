;;; init-pyim --- Init PY Input Method -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Init configuration to use PY Input Method.

;;; Code:

(require 'init-emacs)
(require 'pyim)
(require 'pyim-basedict)
(require 'posframe)
(require 'popon)

(setq! default-input-method "pyim")

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

(setq! pyim-default-scheme 'zirjma)
(setq! pyim-pinyin-fuzzy-alist nil)
(setq! pyim-enable-shortcode nil)
(setq! pyim-candidates-search-buffer-p nil)
(setq! pyim-indicator-list nil)
(setq! pyim-page-tooltip '(posframe popon))

(setq! pyim-punctuation-dict
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

(setq-default pyim-english-input-switch-functions
              '(pyim-probe-program-mode))

(pyim-scheme-add
 `(zirjma
   :document "zirjma"
   :class shuangpin
   :first-chars "abcdefghijklmnopqrstuvwxyz"
   :rest-chars "abcdefghijklmnopqrstuvwxyz"
   :prefer-triggers nil
   :cregexp-support-p t
   :keymaps ,init-pyim-zirjma-keymaps))

(define-key pyim-mode-map "." #'pyim-page-next-page)
(define-key pyim-mode-map "," #'pyim-page-previous-page)

(pyim-basedict-enable)

(provide 'init-pyim)
;;; init-pyim.el ends here
