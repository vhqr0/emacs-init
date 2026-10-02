# header-line-x

Clickable header line buttons to revert or edit the arguments of special
buffers.

## Usage

- occur: Revert, EditRegexp, EditBuffer.
- compilation: Revert, EditCommand, EditDirectory.

```elisp
(add-hook 'occur-mode-hook #'header-line-x-occur-setup)
(add-hook 'compilation-mode-hook #'header-line-x-compile-setup)
```

`header-line-x-button` makes such a button from a label and a command.
