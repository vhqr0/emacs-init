;;; modus-x-zenburn-theme.el --- Modus theme with the colors of Zenburn -*- lexical-binding: t; -*-

;;; Commentary:

;; A dark theme built on `modus-themes', with the colors of Zenburn.
;; The colors not given are derived by `modus-themes-generate-palette'.

;;; Code:

(require 'modus-themes)

(defconst modus-x-zenburn-palette
  (modus-themes-generate-palette
   '((bg-main         "#3f3f3f")
     (fg-main         "#dcdccc")
     (fg-dim          "#989890")
     (bg-hl-line      "#383838")
     (bg-region       "#2b2b2b")
     (red             "#cc9393")
     (green           "#7f9f7f")
     (yellow          "#f0dfaf")
     (blue            "#8cd0d3")
     (magenta         "#dc8cc3")
     (cyan            "#93e0e3")
     (orange          "#dfaf8f")
     (red-intense     "#dca3a3")
     (green-intense   "#9fc59f")
     (green-cooler    "#bfebbf")
     (blue-intense    "#94bff3")
     (blue-faint      "#7cb8bb"))
   nil nil
   '((cursor fg-main)
     (comment green)
     (keyword yellow)
     (builtin fg-main)
     (fnname cyan)
     (type blue-faint)
     (variable orange)
     (constant green-cooler)
     (string red)
     (docstring green-intense)
     (preprocessor blue-intense))))

(modus-themes-theme
 'modus-x-zenburn
 'modus-x-themes
 "Dark theme with the colors of Zenburn."
 'dark
 'modus-x-zenburn-palette
 nil
 nil)

(provide 'modus-x-zenburn-theme)
;;; modus-x-zenburn-theme.el ends here
