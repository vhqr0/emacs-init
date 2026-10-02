# simple-format

Format the buffer with an external program, keeping point and markers.

## Usage

`simple-format-buffer` looks up the command function of the major mode, or of
its nearest ancestor, in `simple-format-command-function-alist`.  It sends the
buffer text to the standard input of the command, and reads the formatted text
from its standard output.  The result is applied by `replace-region-contents`,
which diffs the texts so that point and markers stay in place.  On failure,
the error is shown in `*simple-format errors*` and the buffer is unchanged.
Without a command, the buffer is indented by `indent-region`.  The command
functions below return nil if their programs are not found.

```elisp
(add-to-list 'simple-format-command-function-alist
             '(python-base-mode . my-black-command))
```

## Formatters

| Command function | Command | Default modes |
|---|---|---|
| `simple-format-clang-format-cpp-command` | `clang-format -assume-filename FILE.cpp` | `c-mode`, `c++-mode` |
| `simple-format-clang-format-java-command` | `clang-format -assume-filename FILE.java` | `java-mode` |
| `simple-format-clang-format-csharp-command` | `clang-format -assume-filename FILE.cs` | `csharp-mode` |
| `simple-format-prettier-javascript-command` | `prettier --parser=babel` | `js-base-mode` |
| `simple-format-prettier-typescript-command` | `prettier --parser=typescript` | `typescript-ts-base-mode` |
| `simple-format-prettier-css-command` | `prettier --parser=css` | `css-base-mode` |
| `simple-format-prettier-scss-command` | `prettier --parser=scss` | `scss-mode` |
| `simple-format-prettier-html-command` | `prettier --parser=html` | `html-mode` |
| `simple-format-prettier-json-command` | `prettier --parser=json` | `js-json-mode`, `json-mode` |
| `simple-format-yapf-command` | `yapf` | `python-base-mode` |
| `simple-format-cljfmt-command` | `cljfmt fix -` | `clojure-mode` |

prettier also gets `--stdin-filepath FILE` with a buffer file.  clang-format
infers the language and finds `.clang-format` by the assumed file name, so
only the extension of the buffer file name is replaced, or `stdin.EXT` is
assumed without one.

The programs are customizable by `simple-format-clang-format-program`,
`simple-format-prettier-program`, `simple-format-yapf-program` and
`simple-format-cljfmt-program`.
