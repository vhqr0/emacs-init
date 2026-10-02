# project-test-jump

Jump between source and test files of the current buffer in a project.

## Usage

Bind `project-test-jump`.  It looks up the file extension of the current
buffer in `project-test-jump-function-alist`, a list of `(EXTENSION .
FUNCTION)`, and calls FUNCTION with `default-directory` bound to the project
root.

```elisp
(keymap-set project-prefix-map "t" #'project-test-jump)
```

`project-test-jump-find-file` finds the first existing file in a list, or
creates the first one, which is handy to write such a function.

## Languages

- Clojure (`clj`, `cljc`, `cljs`): `src/foo/bar.clj` <-> `test/foo/bar_test.clj`,
  trying `.clj`, `.cljc` and `.cljs`.
