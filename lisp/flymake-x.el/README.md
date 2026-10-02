# flymake-x

A generic Flymake backend running an external checker on the buffer.

## Usage

Add `flymake-x-backend` to `flymake-diagnostic-functions` locally, such as by
`flymake-x-setup` in a mode hook.  On each check, it
looks up the functions of the major mode, or of its nearest ancestor, in
`flymake-x-function-alist`, a list of `(MAJOR COMMAND-FUNCTION
REPORT-FUNCTION)`:

- COMMAND-FUNCTION returns the checker command, or nil to skip checking.  The
  buffer contents are sent to its standard input.
- REPORT-FUNCTION is called with the source buffer in the checker output
  buffer, and returns a list of Flymake diagnostics.

Without a checker, it reports no diagnostics.

```elisp
(add-hook 'clojure-mode-hook #'flymake-x-setup)
```

## Checkers

| Functions | Checker | Default modes |
|---|---|---|
| `flymake-x-clj-kondo-make-command`, `flymake-x-clj-kondo-make-report` | clj-kondo, by `flymake-x-clj-kondo-program` | `clojure-mode` |
