# simple-abbrev

Define abbrev tables from a data file.

## Usage

`simple-abbrev-file` holds a list of `(TABLENAME (ABBREV . EXPANSION) ...)`:

```elisp
((global-abbrev-table
  ("eamcs" text "emacs"))
 (emacs-lisp-mode-abbrev-table
  ("defun" yas "defun $1 ($2)\n  $0" (ensure-pair t))))
```

EXPANSION is either `(text "expansion")`, or `(yas "snippet" (ENVSYM ENVVAL)
...)` expanding a yasnippet snippet, which requires yasnippet.  With the
`ensure-pair` env, a pair of parens is inserted first unless right after an
open paren, and the snippet expands inside it.

```elisp
(setq simple-abbrev-file (expand-file-name "abbrevs.eld" user-emacs-directory))
(add-hook 'after-init-hook #'simple-abbrev-load)
```
