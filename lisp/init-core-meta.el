;;; -*- lexical-binding: t; no-native-compile: t -*-

(defvar init-metas
  '((init-basic-emacs . ((site gcmh)
                         (elpa undo-tree projectile)))
    (init-basic-edit  . ((elpa smartparens rainbow-delimiters page-break-lines)))
    (init-basic-compl . ((elpa yasnippet auto-yasnippet company)))
    (init-basic-evil  . ((site evil-x)
                         (elpa evil evil-surround evil-snipe evil-multiedit evil-collection)))
    (init-basic-helm  . ((site helm-x)
                         (elpa helm helm-comint helm-ls-git helm-projectile)))
    (init-basic-tools . ((site xdg-open eshell-x)
                         (elpa helpful wgrep wgrep-helm rg magit with-editor format-all)))
    (init-basic-maps  . ((site embark-which-key)
                         (elpa which-key embark)))
    (init-ui-sml      . ((elpa smart-mode-line)))
    (init-cn-pyim     . ((elpa posframe popon pyim pyim-basedict)))
    (init-cn-pyim     . ((elpa fcitx)))
    (init-lang-lisp   . ((elpa evil-cleverparens macrostep)))
    (init-lang-org    . ((elpa evil-org)))
    (init-lang-md     . ((elpa markdown-mode edit-indirect)))
    (init-lang-hy     . ((site hy-python)
                         (elpa hy-mode)))
    (init-lang-clj    . ((elpa clojure-mode cider helm-cider)))
    (init-app-roam    . ((site helm-roam)
                         (elpa org-roam)))))

(provide 'init-core-meta)
