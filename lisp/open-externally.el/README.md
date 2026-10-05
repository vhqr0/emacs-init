# open-externally

Open files, directories and URLs with external applications.

## Usage

- `open-externally` opens the buffer file, or `default-directory` without one.
- `open-externally-at-point` opens the file or URL at point, found by
  `ffap-guesser`.  In Dired, it opens the marked files, or the file at point.

File names are expanded, and URLs are kept as they are.  The opening is done
by `open-externally-function`, `shell-command-do-open` by default, which uses
`shell-command-guess-open`.  Set it to open files differently, such as with
Windows applications in WSL:

```elisp
(setq open-externally-function
      (lambda (files)
        (dolist (file files)
          (call-process "wslview" nil 0 nil file))))
```
