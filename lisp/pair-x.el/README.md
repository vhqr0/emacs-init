# pair-x

Paredit like commands on the list enclosing point, by its syntax.

| Command | Effect |
|---|---|
| `pair-x-splice-sexp` | `(a (b c) d)` → `(a b c d)` |
| `pair-x-forward-slurp-sexp` | `(a (b) c d)` → `(a (b c) d)` |
| `pair-x-forward-barf-sexp` | `(a (b c) d)` → `(a (b) c d)` |
| `pair-x-backward-slurp-sexp` | `(a b (c) d)` → `(a (b c) d)` |
| `pair-x-backward-barf-sexp` | `(a (b c) d)` → `(a b (c) d)` |

Strings are not lists: in a string, the list enclosing the string is edited.
The lines changed are reindented.

```elisp
(keymap-global-set "C-<right>" #'pair-x-forward-slurp-sexp)
(keymap-global-set "C-<left>" #'pair-x-forward-barf-sexp)
(keymap-global-set "C-M-<left>" #'pair-x-backward-slurp-sexp)
(keymap-global-set "C-M-<right>" #'pair-x-backward-barf-sexp)
```
