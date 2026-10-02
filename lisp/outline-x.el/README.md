# outline-x

Outline extensions.

## Usage

- `outline-x-narrow-to-subtree` narrows to the subtree at point.
- Setup functions set the outline headings of a mode:
  - `outline-x-lisp-setup`: `;;;` style comment headings, levels by the number
    of semicolons, and enables `outline-minor-mode`.
  - `outline-x-comint-setup`: prompts of `comint-prompt-regexp`.
  - `outline-x-eshell-setup`: eshell prompts.

```elisp
(keymap-set narrow-map "s" #'outline-x-narrow-to-subtree)
(add-hook 'emacs-lisp-mode-hook #'outline-x-lisp-setup)
(add-hook 'comint-mode-hook #'outline-x-comint-setup)
(add-hook 'eshell-mode-hook #'outline-x-eshell-setup)
```
