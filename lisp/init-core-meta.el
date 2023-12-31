;;; -*- lexical-binding: t; no-native-compile: t -*-

(defvar init-metas
  '((init-basic-emacs . ((elpa embark helpful undo-tree page-break-lines smartparens rainbow-delimiters)
                         (site gcmh)))
    (init-basic-evil  . ((site evil-x)
                         (elpa evil evil-surround evil-collection)))
    (init-basic-helm  . ((site helm-x)
                         (elpa helm helm-comint helm-ls-git)))
    (init-basic-prog  . ((elpa projectile helm-projectile yasnippet company format-all)))
    (init-basic-tools . ((site xdg-open eshell-x)
                         (elpa wgrep wgrep-helm rg magit with-editor)))
    (init-ui-sml      . ((elpa smart-mode-line)))
    (init-cn-pyim     . ((elpa posframe popon pyim pyim-basedict)))
    (init-lang-lisp   . ((elpa evil-cleverparens macrostep)))
    (init-lang-org    . ((elpa evil-org)))
    (init-lang-md     . ((elpa markdown-mode edit-indirect)))
    (init-lang-hy     . ((site hy-python)
                         (elpa hy-mode)))
    (init-lang-clj    . ((elpa clojure-mode cider)))
    (init-app-roam    . ((site helm-roam)
                         (elpa org-roam)))))

(provide 'init-core-meta)
