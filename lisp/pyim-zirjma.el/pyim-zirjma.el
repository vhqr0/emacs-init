;;; pyim-zirjma.el --- Zirjma shuangpin scheme for pyim -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; Package-Requires: ((emacs "31.1") (pyim "5.0"))
;; Version: 0.1.0
;; Keywords: i18n

;;; Commentary:

;; The zirjma shuangpin scheme for pyim.  Load this package, then set
;; `pyim-default-scheme' to `zirjma'.

;;; Code:

(require 'pyim)

(defvar pyim-zirjma-keymaps
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
    ("ou"   "ou"                ))
  "Keymaps of the zirjma shuangpin scheme.")

(pyim-scheme-add
 `(zirjma
   :document "zirjma"
   :class shuangpin
   :first-chars "abcdefghijklmnopqrstuvwxyz"
   :rest-chars "abcdefghijklmnopqrstuvwxyz"
   :prefer-triggers nil
   :cregexp-support-p t
   :keymaps ,pyim-zirjma-keymaps))

(provide 'pyim-zirjma)
;;; pyim-zirjma.el ends here
