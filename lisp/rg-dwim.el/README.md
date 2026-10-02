# rg-dwim

Search with ripgrep in a `grep-mode` buffer, defaulting to the symbol at
point.

## Usage

`M-x rg-dwim`:

- Without prefix argument, search in the project root.
- With one `C-u`, prompt for the directory.
- With two `C-u`, also edit the command.

The program is `rg-dwim-program`.
