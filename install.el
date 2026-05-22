(defvar init-packages
  '(
    evil
    evil-surround
    paredit
    avy
    hydra
    amx
    ivy
    ivy-avy
    ivy-hydra
    swiper
    counsel
    yasnippet
    company
    apheleia
    wgrep
    git-modes
    with-editor
    magit
    orgit
    clojure-mode
    cider
    org-roam
    markdown-mode
    edit-indirect
    pyim
    pyim-basedict
    posframe
    ))

(require 'package)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

(package-refresh-contents)

(dolist (package init-packages)
  (package-install package))
