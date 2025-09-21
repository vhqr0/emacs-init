;;; init-clojure.el --- Init Clojure -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Clojure language.

;;; Code:

(require 'init-emacs)

;;; major mode

(require 'clojure-mode)

(defvar init-clojure-modes
  '(clojurec-mode clojure-mode clojurescript-mode))

(add-hook 'clojure-mode-hook #'init-lisp-set-outline)

(defun init-clojure-set-elec-pairs ()
  "Set `electric-pair-pairs' for Clojure mode."
  (setq-local electric-pair-pairs
              (add-to-list 'electric-pair-pairs '(?` . ?`))))

(add-hook 'clojure-mode-hook #'init-clojure-set-elec-pairs)

(defun init-clojure-remove-comma-dwim ()
  "Remove comma dwim."
  (interactive)
  (let ((bounds (or (init-region-bounds) (bounds-of-thing-at-point 'sexp))))
    (replace-string-in-region "," "" (car bounds) (cdr bounds))))

(keymap-set clojure-refactor-map ":" #'clojure-toggle-keyword-string)
(keymap-set clojure-refactor-map "," #'init-clojure-remove-comma-dwim)

;;;; find test file

(defvar init-clojure-try-extensions '("cljc" "clj" "cljs"))

(defun init-clojure-try-extensions (extension)
  "Return try extensions with prefer EXTENSION."
  (cons extension (remove extension init-clojure-try-extensions)))

(defun init-clojure-first-test-file-name (file-name)
  "Convert FILE-NAME to test or source file name with the first try extension."
  (cond
   ((string-match "^src/\\(.*\\)\\(\\.clj.?\\)$" file-name)
    (concat "test/" (match-string 1 file-name) "_test" (match-string 2 file-name)))
   ((string-match "^test/\\(.*\\)_test\\(\\.clj.?\\)$" file-name)
    (concat "src/" (match-string 1 file-name) (match-string 2 file-name)))))

(defun init-clojure-find-test-file-name (file-name)
  "Find test file or source file of FILE-NAME."
  (when-let* ((first-test-file-name (init-clojure-first-test-file-name file-name)))
    (let* ((file-name-base (file-name-sans-extension first-test-file-name))
           (try-extensions (init-clojure-try-extensions (file-name-extension first-test-file-name)))
           (try-file-names (->> try-extensions (--map (concat file-name-base "." it)))))
      (->> try-file-names (-first #'file-exists-p)))))

(defun init-clojure-set-find-test-file ()
  "Set `init-find-test-file-name' for Clojure mode."
  (setq-local init-find-test-file-name-function #'init-clojure-find-test-file-name))

(add-hook 'clojure-mode-hook #'init-clojure-set-find-test-file)

;;; cider

(require 'cider)
(require 'cider-format)
(require 'cider-macroexpansion)

(setq cider-mode-line '(:eval (format " Cider[%s]" (cider--modeline-info))))

(defvar init-cider-last-sexp-commands
  (list #'cider-eval-last-sexp
        #'cider-eval-last-sexp-to-repl
        #'cider-eval-last-sexp-in-context
        #'cider-eval-last-sexp-and-replace
        #'cider-pprint-eval-last-sexp
        #'cider-pprint-eval-last-sexp-to-repl
        #'cider-pprint-eval-last-sexp-to-comment
        #'cider-insert-last-sexp-in-repl
        #'cider-tap-last-sexp
        #'cider-format-edn-last-sexp
        #'cider-inspect-last-sexp
        #'cider-macroexpand-1
        #'cider-macroexpand-all
        #'cider-macroexpand-1-inplace
        #'cider-macroexpand-all-inplace))

(dolist (command init-cider-last-sexp-commands)
  (advice-add command :around #'init-lisp-around-last-sexp-maybe-forward))

;;;; eval

(dolist (mode init-clojure-modes)
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
  (let ((bounds (init-region-or-buffer-bounds)))
    (cider-format-region (car bounds) (cdr bounds))))

(keymap-set cider-mode-map "<remap> <init-indent-dwim>" #'init-cider-format-dwim)

(dolist (map (list cider-mode-map cider-repl-mode-map))
  (keymap-set map "C-M-q" #'cider-format-edn-last-sexp))

;;;; repl

(add-to-list 'consult-mode-histories
             '(cider-repl-mode cider-repl-input-history cider-repl-input-history-position cider-repl-bol-mark))

(evil-define-key 'insert cider-repl-mode-map
  (kbd "M-r") #'consult-history)

(evil-define-key 'motion cider-repl-mode-map
  (kbd "RET") #'cider-repl-return
  (kbd "<return>") #'cider-repl-return
  "gj" #'cider-repl-next-prompt
  "gk" #'cider-repl-previous-prompt
  (kbd "C-j") #'cider-repl-next-prompt
  (kbd "C-k") #'cider-repl-previous-prompt)

(provide 'init-clojure)
;;; init-clojure.el ends here
