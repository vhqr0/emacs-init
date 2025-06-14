;;; init-clojure.el --- Init Clojure -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Clojure language.

;;; Code:

(require 'init-emacs)



;;; major mode

(require 'clojure-mode)

(defvar init-clojure-modes
  '(clojurec-mode clojure-mode clojurescript-mode))

(add-hook 'clojure-mode-hook #'init-lisp-common-set-outline)

(defun init-clojure-set-elec-pairs ()
  "Set `electric-pair-pairs' for Clojure mode."
  (setq-local electric-pair-pairs
              (add-to-list 'electric-pair-pairs '(?` . ?`))))

(add-hook 'clojure-mode-hook #'init-clojure-set-elec-pairs)

(defun init-clojure-remove-comma-dwim ()
  "Remove comma dwim."
  (interactive)
  (let ((region (if (region-active-p)
                    (cons (region-beginning) (region-end))
                  (cons (line-beginning-position) (line-end-position)))))
    (replace-string-in-region "," "" (car region) (cdr region))))

(keymap-set clojure-refactor-map "SPC" #'clojure-align)
(keymap-set clojure-refactor-map ":" #'clojure-toggle-keyword-string)
(keymap-set clojure-refactor-map "," #'init-clojure-remove-comma-dwim)

;;;; test jump

(defvar init-clojure-try-extensions '("cljc" "clj" "cljs"))

(defun init-clojure-try-extensions (extension)
  "Return try extensions with prefer EXTENSION."
  (cons extension (remove extension init-clojure-try-extensions)))

(defun init-clojure-test-file-name (file-name)
  "Convert FILE-NAME between src and test file."
  (cond
   ((string-match "^src/\\(.*\\)\\(\\.clj.?\\)$" file-name)
    (concat "test/" (match-string 1 file-name) "_test" (match-string 2 file-name)))
   ((string-match "^test/\\(.*\\)_test\\(\\.clj.?\\)$" file-name)
    (concat "src/" (match-string 1 file-name) (match-string 2 file-name)))))

(defun init-clojure-test-jump ()
  "Jump to test or src file of current file."
  (interactive)
  (-when-let (default-directory (init-project-directory))
    (-when-let (file-name (buffer-file-name))
      (let ((file-name (f-relative file-name)))
        (-when-let (file-name (init-clojure-test-file-name file-name))
          (let* ((file-name-base (f-no-ext file-name))
                 (try-extensions (init-clojure-try-extensions (f-ext file-name)))
                 (try-file-names (->> try-extensions (--map (concat file-name-base "." it)))))
            (-if-let (file-name (->> try-file-names (-first #'f-exists?)))
                (find-file file-name)
              (user-error "Jump to test failed"))))))))

(keymap-set clojure-mode-map "C-x p t" #'init-clojure-test-jump)

(init-leader-set clojure-mode-map
  "p t" #'init-clojure-test-jump)



;;; cider

(require 'cider)
(require 'cider-format)
(require 'cider-macroexpansion)

(setq cider-mode-line '(:eval (format " Cider[%s]" (cider--modeline-info))))

(dolist (map (list cider-mode-map cider-repl-mode-map))
  (define-key map [remap evil-lookup] #'cider-doc)
  (define-key map [remap evil-goto-definition] #'xref-find-definitions))

(dolist (mode init-clojure-modes)
  (add-to-list 'init-evil-eval-function-alist `(,mode . cider-eval-region)))

(defvar init-cider-around-last-sexp-commands
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

(dolist (command init-cider-around-last-sexp-commands)
  (advice-add command :around #'init-lisp-common-around-last-sexp))

(keymap-set cider-mode-map "C-c C-n" #'cider-repl-set-ns)
(keymap-set cider-mode-map "C-c C-i" #'cider-insert-last-sexp-in-repl)
(keymap-set cider-mode-map "C-c C-;" #'cider-pprint-eval-last-sexp-to-comment)

(dolist (map (list cider-mode-map cider-repl-mode-map))
  (keymap-set map "C-M-q" #'cider-format-edn-last-sexp))

(defun init-cider-repl-set-xref ()
  "Set Xref backend for Cider REPL."
  (add-hook 'xref-backend-functions #'cider--xref-backend nil t))

(add-hook 'cider-repl-mode-hook #'init-cider-repl-set-xref)

;;;; history

(add-to-list 'consult-mode-histories
             '(cider-repl-mode cider-repl-input-history cider-repl-input-history-position cider-repl-bol-mark))

(evil-define-key 'insert cider-repl-mode-map
  (kbd "M-r") #'consult-history)

(provide 'init-clojure)
;;; init-clojure.el ends here
