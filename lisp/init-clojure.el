;;; init-clojure.el --- Init Clojure -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Clojure language.

;;; Code:

(require 'init-emacs)



;;; major mode

(require 'clojure-mode)

(defvar init-clojure-modes
  '(clojurec-mode clojure-mode clojurescript-mode))

(add-hook 'clojure-mode-hook #'init-elisp-set-outline)

(defun init-clojure-remove-comma-dwim ()
  "Remove comma dwim."
  (interactive)
  (let ((region (if (region-active-p)
                    (cons (region-beginning) (region-end))
                  (cons (line-beginning-position) (line-end-position)))))
    (replace-string-in-region "," "" (car region) (cdr region))))

(keymap-set clojure-refactor-map ":" #'clojure-toggle-keyword-string)
(keymap-set clojure-refactor-map "SPC" #'clojure-align)
(keymap-set clojure-refactor-map "," #'init-clojure-remove-comma-dwim)

(evil-define-key 'normal clojure-mode-map
  "gr" clojure-refactor-map)

(defun init-clojure-test-file (file)
  "Get test or src of FILE, or nil."
  (cond
   ((string-match "^src/\\(.*\\)\\(\\.clj.?\\)$" file)
    (concat "test/" (match-string 1 file) "_test" (match-string 2 file)))
   ((string-match "^test/\\(.*\\)_test\\(\\.clj.?\\)$" file)
    (concat "src/" (match-string 1 file) (match-string 2 file)))))

(defun init-clojure-test-jump (&optional arg)
  "Jump to test or src of current file.
ARG see `init-dwim-project-find-file'."
  (interactive "P")
  (-when-let (file (buffer-file-name))
    (-when-let (file (init-project-file-relative-name file))
      (-when-let (file (init-clojure-test-file file))
        (init-dwim-project-find-file arg file)))))

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

(defun init-cider-around-last-sexp (func &rest args)
  "Around Cider *-last-sexp command.
Save point and forward sexp before command if looking at an open paren.
FUNC and ARGS see specific command."
  (save-excursion
    (when (looking-at-p "(")
      (forward-sexp))
    (apply func args)))

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
  (advice-add command :around #'init-cider-around-last-sexp))

(keymap-set cider-mode-map "C-c C-n" #'cider-repl-set-ns)
(keymap-set cider-mode-map "C-c C-i" #'cider-insert-last-sexp-in-repl)
(keymap-set cider-mode-map "C-c C-;" #'cider-pprint-eval-last-sexp-to-comment)

(dolist (map (list cider-mode-map cider-repl-mode-map))
  (keymap-set map "C-M-q" #'cider-format-edn-last-sexp))

;;;; consult history

(add-to-list 'consult-mode-histories
             '(cider-repl-mode cider-repl-input-history cider-repl-input-history-position cider-repl-bol-mark))

(evil-define-key 'insert cider-repl-mode-map
  (kbd "M-r") #'consult-history)

;;;; macrostep

(defun init-cider-macrostep-macro-form-p (_sexp _env)
  "Macro?"
  t)

(defun init-cider-macrostep-sexp-bounds ()
  "Find bounds of macro sexp."
  (interactive)
  (bounds-of-thing-at-point 'sexp))

(defun init-cider-macrostep-expand (sexp _env)
  "Expand SEXP using Cider."
  (or (cider-sync-request:macroexpand "macroexpand" sexp)
      (user-error "Macro expansion failed")))

(defun init-cider-macrostep-expand-1 (sexp _env)
  "Expand SEXP using Cider."
  (or (cider-sync-request:macroexpand "macroexpand-1" sexp)
      (user-error "Macro expansion failed")))

(defun init-cider-macrostep-insert (sexp _env)
  "Insert expanded SEXP."
  (insert (propertize sexp 'face 'macrostep-expansion-highlight-face)))

(defun init-cider-set-macrostep ()
  "Set Cider macroexpand backends."
  (setq-local macrostep-environment-at-point-function #'ignore)
  (setq-local macrostep-macro-form-p-function #'init-cider-macrostep-macro-form-p)
  (setq-local macrostep-sexp-bounds-function #'init-cider-macrostep-sexp-bounds)
  (setq-local macrostep-sexp-at-point-function #'buffer-substring-no-properties)
  (setq-local macrostep-expand-function #'init-cider-macrostep-expand)
  (setq-local macrostep-expand-1-function #'init-cider-macrostep-expand-1)
  (setq-local macrostep-print-function #'init-cider-macrostep-insert))

(dolist (hook '(cider-mode-hook cider-repl-mode-hook))
  (add-hook hook #'init-cider-set-macrostep))

(dolist (map (list cider-mode-map cider-repl-mode-map))
  (keymap-set map "C-c e" #'macrostep-expand))

(provide 'init-clojure)
;;; init-clojure.el ends here
