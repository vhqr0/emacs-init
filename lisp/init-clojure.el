;;; init-clojure.el --- Init Clojure -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Clojure language.

;;; Code:

(require 'init-emacs)
(require 'clojure-mode)

(add-hook 'clojure-mode-hook #'init-lisp-set-outline)

(defun init-clojure-set-elec-pairs ()
  "Set `electric-pair-pairs' for Clojure mode."
  (setq-local electric-pair-pairs
              (add-to-list 'electric-pair-pairs '(?` . ?`))))

(add-hook 'clojure-mode-hook #'init-clojure-set-elec-pairs)

(defun init-clojure-remove-comma-dwim ()
  "Remove comma dwim."
  (interactive)
  (let ((bounds (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (bounds-of-thing-at-point 'sexp))))
    (replace-string-in-region "," "" (car bounds) (cdr bounds))))

(keymap-set clojure-refactor-map "," #'init-clojure-remove-comma-dwim)

(defvar init-clojure-extensions '("cljc" "clj" "cljs"))

(defun init-clojure-extensions (extension)
  "Return clojure file extensions, given EXTENSION first."
  (cons extension (remove extension init-clojure-extensions)))

;; (init-clojure-extensions "clj") => '("clj" "cljc" "cljs")
;; (init-clojure-extensions "cljc") => '("cljc" "clj" "cljs")

(defun init-clojure-file-with-extensions (file)
  "Return clojure files with different extension, given FILE first."
  (let ((base (file-name-sans-extension file))
        (extension (file-name-extension file)))
    (thread-last
      (init-clojure-extensions extension)
      (seq-map (lambda (extension) (concat base "." extension))))))

;; (init-clojure-file-with-extensions "foo/bar.clj")
;; => '("foo/bar.clj" "foo/bar.cljc" "foo/bar.cljs")
;; (init-clojure-file-with-extensions "foo/bar.cljc")
;; => '("foo/bar.cljc" "foo/bar.clj" "foo/bar.cljs")

(defun init-clojure-test-file (file)
  "Convert FILE to test file with same extension."
  (let ((file (concat "/" file)))
    (cond
     ((string-match "\\(.*?\\)/src/\\(.*\\)\\(\\.clj.?\\)$" file)
      (thread-first
        (concat (match-string 1 file) "/test/" (match-string 2 file) "_test" (match-string 3 file))
        (substring 1)))
     ((string-match "\\(.*?\\)/test/\\(.*\\)_test\\(\\.clj.?\\)$" file)
      (thread-first
        (concat (match-string 1 file) "/src/" (match-string 2 file) (match-string 3 file))
        (substring 1))))))

;; (init-clojure-test-file "src/foo/bar.clj") => "test/foo/bar_test.clj"
;; (init-clojure-test-file "test/foo/bar_test.clj") => "src/foo/bar.clj"
;; (init-clojure-test-file "clojure/src/foo/bar.clj") => "clojure/test/foo/bar_test.clj"
;; (init-clojure-test-file "clojure/test/foo/bar_test.clj") => "clojure/src/foo/bar.clj"

(defun init-clojure-test-files (file)
  "Convert FILE to test files, with possible extensions."
  (when-let* ((file (init-clojure-test-file file)))
    (init-clojure-file-with-extensions file)))

;; (init-clojure-test-files "src/foo/bar.clj")
;; => '("test/foo/bar_test.clj" "test/foo/bar_test.cljc" "test/foo/bar_test.cljs")
;; (init-clojure-test-files "test/foo/bar_test.cljc")
;; => '("src/foo/bar.cljc" "src/foo/bar.clj" "src/foo/bar.cljs")

(defun init-clojure-find-test-file ()
  "Find test file of current buffer."
  (if (not buffer-file-name)
      (user-error "No buffer file name found")
    (let* ((file (file-relative-name buffer-file-name))
           (files (init-clojure-test-files file)))
      (if-let* ((file (seq-find #'file-exists-p files)))
          (find-file file)
        (if-let* ((file (car files)))
            (find-file (read-file-name
                        "Create test file: "
                        (file-name-directory file)
                        nil nil
                        (file-name-nondirectory file)))
          (user-error "No test file found"))))))

(defun init-clojure-set-find-test-file ()
  "Set `init-find-test-file' for Clojure mode."
  (setq-local init-find-test-file-function #'init-clojure-find-test-file))

(add-hook 'clojure-mode-hook #'init-clojure-set-find-test-file)

;;; cider

(require 'cider)

(setq cider-mode-line '(:eval (format " Cider[%s]" (cider--modeline-info))))

(init-define-next-sexp-command cider-eval-last-sexp)
(init-define-next-sexp-command cider-eval-last-sexp-to-repl)
(init-define-next-sexp-command cider-eval-last-sexp-in-context)
(init-define-next-sexp-command cider-eval-last-sexp-and-replace)
(init-define-next-sexp-command cider-pprint-eval-last-sexp)
(init-define-next-sexp-command cider-pprint-eval-last-sexp-to-repl)
(init-define-next-sexp-command cider-pprint-eval-last-sexp-to-comment)
(init-define-next-sexp-command cider-insert-last-sexp-in-repl)
(init-define-next-sexp-command cider-tap-last-sexp)
(init-define-next-sexp-command cider-format-edn-last-sexp)
(init-define-next-sexp-command cider-inspect-last-sexp)
(init-define-next-sexp-command cider-macroexpand-1)
(init-define-next-sexp-command cider-macroexpand-all)
(init-define-next-sexp-command cider-macroexpand-1-inplace)
(init-define-next-sexp-command cider-macroexpand-all-inplace)

(keymap-set cider-mode-map "C-c C-n" #'cider-repl-set-ns)
(keymap-set cider-mode-map "C-c C-i" #'cider-insert-last-sexp-in-repl)
(keymap-set cider-mode-map "C-c C-;" #'cider-pprint-eval-last-sexp-to-comment)

(dolist (mode '(clojurec-mode clojure-mode clojurescript-mode))
  (add-to-list 'init-evil-eval-function-alist `(,mode . cider-eval-region)))

(defun init-counsel-cider-repl-history ()
  "Browse Cider REPL history."
  (interactive)
  (setq ivy-completion-beg (point))
  (setq ivy-completion-end (point))
  (ivy-read "History: " (ivy-history-contents cider-repl-input-history)
            :keymap ivy-reverse-i-search-map
            :action #'counsel--browse-history-action
            :caller #'init-counsel-cider-repl-history))

(dolist (map (list cider-mode-map cider-repl-mode-map))
  (keymap-set map "C-M-q" #'cider-format-edn-last-sexp)
  (keymap-set map "<remap> <evil-lookup>" #'cider-doc)
  (keymap-set map "<remap> <init-history-placeholder>" #'init-counsel-cider-repl-history))

(defun init-cider-repl-set-xref ()
  "Set Xref backend for Cider REPL."
  (add-hook 'xref-backend-functions #'cider--xref-backend nil t))

(add-hook 'cider-repl-mode-hook #'init-cider-repl-set-xref)

(provide 'init-clojure)
;;; init-clojure.el ends here
