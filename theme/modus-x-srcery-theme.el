;;; modus-x-srcery-theme.el --- Modus theme with the colors of Srcery -*- lexical-binding: t; -*-

;;; Commentary:

;; A dark theme built on `modus-themes', with the colors of Srcery.
;; The colors not given are derived by `modus-themes-generate-palette'.

;;; Code:

(require 'modus-themes)

(defconst modus-x-srcery-palette
  (modus-themes-generate-palette
   '((bg-main         "#1c1b19")
     (fg-main         "#fce8c3")
     (fg-dim          "#918175")
     (fg-alt          "#baa67f")
     (red             "#ef2f27")
     (green           "#519f50")
     (yellow          "#fbb829")
     (blue            "#2c78bf")
     (magenta         "#e02c6d")
     (cyan            "#0aaeb3")
     (red-intense     "#f75341")
     (green-intense   "#98bc37")
     (yellow-intense  "#fed06e")
     (blue-intense    "#68a8e4")
     (magenta-intense "#ff5c8f")
     (cyan-intense    "#2be4d0"))
   nil nil
   '((cursor yellow)
     (comment fg-dim)
     (keyword red)
     (builtin blue)
     (fnname yellow)
     (type blue-intense)
     (variable cyan)
     (constant magenta-intense)
     (string green-intense)
     (docstring green))))

(modus-themes-theme
 'modus-x-srcery
 'modus-x-themes
 "Dark theme with the colors of Srcery."
 'dark
 'modus-x-srcery-palette
 nil
 nil)

(provide 'modus-x-srcery-theme)
;;; modus-x-srcery-theme.el ends here
