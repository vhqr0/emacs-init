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

(defun init-clojure-test-file (file)
  "Convert FILE to test file with same extension."
  (cond
   ((string-match "^src/\\(.*\\)\\(\\.clj.?\\)$" file)
    (concat "test/" (match-string 1 file) "_test" (match-string 2 file)))
   ((string-match "^test/\\(.*\\)_test\\(\\.clj.?\\)$" file)
    (concat "src/" (match-string 1 file) (match-string 2 file)))))

(defun init-clojure-test-files (file)
  "Convert FILE to test files, with possible extensions."
  (when-let* ((file (init-clojure-test-file file)))
    (let ((base (file-name-sans-extension file))
          (extension (file-name-extension file)))
      (seq-map
       (lambda (extension) (concat base "." extension))
       (init-clojure-extensions extension)))))

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
(require 'cider-format)
(require 'cider-macroexpansion)

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

;;;; eval

(dolist (mode '(clojurec-mode clojure-mode clojurescript-mode))
  (add-to-list 'init-evil-eval-function-alist `(,mode . cider-eval-region)))

(keymap-set cider-mode-map "C-c C-n" #'cider-repl-set-ns)
(keymap-set cider-mode-map "C-c C-i" #'cider-insert-last-sexp-in-repl)
(keymap-set cider-mode-map "C-c C-;" #'cider-pprint-eval-last-sexp-to-comment)

;;;; lookup

(dolist (map (list cider-mode-map cider-repl-mode-map))
  (keymap-set map "<remap> <evil-lookup>" #'cider-doc))

(defun init-cider-repl-set-xref ()
  "Set Xref backend for Cider REPL."
  (add-hook 'xref-backend-functions #'cider--xref-backend nil t))

(add-hook 'cider-repl-mode-hook #'init-cider-repl-set-xref)

;;;; format

(defun init-cider-format-dwim ()
  "Do Cider format smartly."
  (interactive)
  (if (use-region-p)
      (call-interactively #'cider-format-region)
    (call-interactively #'cider-format-buffer)))

(keymap-set cider-mode-map "<remap> <init-indent-dwim>" #'init-cider-format-dwim)

(dolist (map (list cider-mode-map cider-repl-mode-map))
  (keymap-set map "C-M-q" #'cider-format-edn-last-sexp))

;;;; repl

(add-to-list 'consult-mode-histories
             '(cider-repl-mode cider-repl-input-history cider-repl-input-history-position cider-repl-bol-mark))

(provide 'init-clojure)
;;; init-clojure.el ends here
