# flymake-x

A generic Flymake backend running an external checker on the buffer.

## Usage

Set two functions locally, then add `flymake-x-backend` to
`flymake-diagnostic-functions`:

- `flymake-x-make-command-function` returns the checker command, or nil to
  skip checking.  The buffer contents are sent to its standard input.
- `flymake-x-make-report-function` is called with the source buffer in the
  checker output buffer, and returns a list of Flymake diagnostics.

```elisp
(defun my-checker-setup ()
  (setq-local flymake-x-make-command-function #'my-checker-command)
  (setq-local flymake-x-make-report-function #'my-checker-report)
  (add-hook 'flymake-diagnostic-functions #'flymake-x-backend nil t))
```

## Checkers

- clj-kondo: lint Clojure with `flymake-x-clj-kondo-program`.

  ```elisp
  (add-hook 'clojure-mode-hook #'flymake-x-clj-kondo-setup)
  ```
