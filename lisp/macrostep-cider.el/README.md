# macrostep-cider

Expand Clojure macros with `macrostep-expand` through Cider.

## Usage

```elisp
(dolist (hook '(cider-mode-hook cider-repl-mode-hook))
  (add-hook hook #'macrostep-cider-setup))
```
