;;; modus-candy-theme.el --- Candy colored dark theme -*- lexical-binding: t; -*-

;;; Commentary:

;; A dark derivative of the Modus themes with candy colors.  The palette
;; is generated from a few base colors by
;; `modus-themes-generate-palette'.

;;; Code:

(eval-and-compile
  (require-theme 'modus-themes))

(defvar modus-candy-palette
  (modus-themes-generate-palette
   '((bg-main "#160b24")
     (fg-main "#f2eaff")
     (red "#ff3333")
     (green "#00c900")
     (yellow "#ffb500")
     (blue "#0078ff")
     (magenta "#c600ff"))
   'cool)
  "Palette of `modus-candy'.")

(defvar modus-candy-palette-user
  '((purple "#5b00ae"))
  "Named colors added to `modus-candy-palette'.")

(defvar modus-candy-palette-overrides
  '((cursor magenta)
    (bg-region purple)
    (bg-mode-line-active purple)
    (border-mode-line-active magenta)
    (keyword magenta)
    (fnname blue-warmer)
    (type yellow)
    (string green)
    (docstring green-faint)
    (info green))
  "Overrides for `modus-candy-palette'.")

(modus-themes-theme
 'modus-candy
 'modus-candy
 "Candy colored dark theme based on the Modus themes."
 'dark
 'modus-candy-palette
 'modus-candy-palette-user
 'modus-candy-palette-overrides)

;;; modus-candy-theme.el ends here
