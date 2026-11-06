# search-x

Tools to revert or edit the arguments of occur and compilation buffers, by
clickable header line buttons and commands.

## Usage

- occur: Revert, EditRegexp, EditBuffer.
- compilation: Revert, EditCommand, EditDirectory.

```elisp
(add-hook 'occur-mode-hook #'search-x-occur-header-setup)
(add-hook 'compilation-mode-hook #'search-x-compile-header-setup)
(keymap-set occur-mode-map "/" #'search-x-occur-edit-regexp)
(keymap-set occur-mode-map "C-c C-/" #'search-x-occur-edit-regexp)
(keymap-set compilation-mode-map "/" #'search-x-compile-edit-command)
(keymap-set compilation-mode-map "C-c C-/" #'search-x-compile-edit-command)
(keymap-set compilation-minor-mode-map "/" #'search-x-compile-edit-command)
(keymap-set compilation-minor-mode-map "C-c C-/" #'search-x-compile-edit-command)
```

`compilation-mode-map` does not inherit `compilation-minor-mode-map`, which
`grep-mode-map` inherits.

`search-x-header-button` makes such a button from a label and a command.
