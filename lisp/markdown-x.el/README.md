# markdown-x

Org-like extensions for markdown.

## Todo

`markdown-x-toggle-todo` toggles `TODO ` after the `#` marks of the heading at
the current line.

## Capture

`markdown-x-capture` reads a char, then captures a note by the template of it
in `markdown-x-capture-templates`, a list of `(CHAR DESCRIPTION FILE
TEMPLATE)`.  TEMPLATE is a yasnippet snippet, expanded in a temporary buffer of
`markdown-x-capture-major-mode`, `text-mode` by default.  `C-c C-c` appends
the buffer to FILE, and `C-c C-k` aborts.  Templates are expanded literally,
without reindenting.

Without templates, `markdown-x-capture` captures to
`markdown-x-capture-default-file` (`~/.emacs.d/notes.md`) by
`markdown-x-capture-default-template`: a TODO heading, the current time, the
file and line where capture started, and the text of the line as indented
code.

```elisp
(setq markdown-x-capture-major-mode #'markdown-mode)
(setq markdown-x-capture-templates
      '((?t "Todo" "~/notes/inbox.md"
            "## TODO $0\n`(format-time-string \"[%F %a %R]\")`\n")))
```

Templates may use these variables in backquoted elisp, describing where capture
started:

- `markdown-x-capture-origin-buffer`: the buffer.
- `markdown-x-capture-origin-file`: its file name, or nil.
- `markdown-x-capture-origin-line`: the line number.
- `markdown-x-capture-origin-line-text`: the text of the line.
- `markdown-x-capture-origin-region`: the text of the active region, or nil.

```elisp
(?a "Todo with link" "~/notes/inbox.md"
    "## TODO $0\n[`markdown-x-capture-origin-line-text`](`markdown-x-capture-origin-file`)\n")
```

## Agenda

`markdown-x-agenda-files` lists files or directories, a directory standing for
the markdown files in it, defaulting to `markdown-x-capture-default-file`.
`markdown-x-agenda-headings` collects their headings as `(FILE LINE TEXT)`,
skipping fenced code blocks.

`markdown-x-agenda-todo` lists the TODO headings:

- `RET` goes to the heading, `o` in other window.
- `C-c C-t` toggles TODO of the heading.
- `g` regenerates the list.

## Test

```sh
make test
```
