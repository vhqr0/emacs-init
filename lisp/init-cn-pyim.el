;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)

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

(defvar init-pyim-punctuation-dict
  '(("'"  "‘"  "’")
    ("\"" "“"  "”")
    ("("  "（"    )
    (")"  "）"    )
    ("["  "【"    )
    ("]"  "】"    )
    ("<"  "《"    )
    (">"  "》"    )
    ("?"  "？"    )
    ("!"  "！"    )
    (","  "，"    )
    ("."  "。"    )
    (";"  "；"    )
    (":"  "："    )
    ("\\" "、"    )))

(init-setq-declare!
 pyim-default-scheme 'zirjma
 pyim-pinyin-fuzzy-alist nil
 pyim-enable-shortcode nil
 pyim-candidates-search-buffer-p nil
 pyim-indicator-list nil
 pyim-punctuation-dict init-pyim-punctuation-dict
 pyim-page-tooltip '(posframe popon))

(defvar pyim-mode-map)
(declare-function pyim-scheme-add "pyim")
(declare-function pyim-page-next-page "pyim")
(declare-function pyim-page-previous-page "pyim")

(with-eval-after-load 'pyim
  (require 'posframe)
  (require 'popon)
  (pyim-scheme-add
   `(zirjma
     :document "zirjma"
     :class shuangpin
     :first-chars "abcdefghijklmnopqrstuvwxyz"
     :rest-chars "abcdefghijklmnopqrstuvwxyz"
     :prefer-triggers nil
     :cregexp-support-p t
     :keymaps ,init-pyim-zirjma-keymaps))
  (init-define-key
   pyim-mode-map
   "." #'pyim-page-next-page
   "," #'pyim-page-previous-page)
  (pyim-basedict-enable))

(provide 'init-cn-pyim)
