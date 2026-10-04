# modus-themes-x

Themes built on top of the Modus themes, each generating its palette from a
few base colors by `modus-themes-generate-palette`.

## Themes

- `modus-candy`: a dark theme with candy colors.

## Usage

Add this directory to `custom-theme-load-path`, then load a theme:

```elisp
(add-to-list 'custom-theme-load-path "/path/to/modus-themes-x.el")
(load-theme 'modus-candy t)
```
