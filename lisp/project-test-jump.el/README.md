# project-test-jump

Jump between source and test files of the current buffer in a project.

## Usage

Bind `project-test-jump`, and set `project-test-jump-function` locally in a
major mode hook.  The function is called with `default-directory` bound to the
project root.

```elisp
(keymap-set project-prefix-map "t" #'project-test-jump)
```

`project-test-jump-find-file` finds the first existing file in a list, or
creates the first one, which is handy to write such a function.

## Languages

- Clojure: `src/foo/bar.clj` <-> `test/foo/bar_test.clj`, trying `.clj`,
  `.cljc` and `.cljs`.

  ```elisp
  (add-hook 'clojure-mode-hook #'project-test-jump-clojure-setup)
  ```
