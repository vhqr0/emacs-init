;;; init-clojure.el --- Init Clojure -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Clojure language.

;;; Code:

(require 'init-emacs)



;;; major mode

(require 'clojure-mode)

(defvar init-clojure-modes
  '(clojurec-mode clojure-mode clojurescript-mode))

(defvar init-clojure-extensions '("cljc" "clj" "cljs"))

(defun init-clojure-try-extensions (extension)
  "Return try extensions with prefer EXTENSION."
  (cons extension (remove extension init-clojure-extensions)))

(add-hook 'clojure-mode-hook #'init-elisp-set-outline)

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

(defun init-clojure-test-jump ()
  "Jump to test or src file of current file."
  (interactive)
  (-when-let (default-directory (init-project-directory))
    (-when-let (file-name (buffer-file-name))
      (let ((file-name (f-relative file-name)))
        (-when-let (file-name (cond
                               ((string-match "^src/\\(.*\\)\\(\\.clj.?\\)$" file-name)
                                (concat "test/" (match-string 1 file-name) "_test" (match-string 2 file-name)))
                               ((string-match "^test/\\(.*\\)_test\\(\\.clj.?\\)$" file-name)
                                (concat "src/" (match-string 1 file-name) (match-string 2 file-name)))))
          (let* ((file-name-base (f-no-ext file-name))
                 (extension (f-ext file-name))
                 (extensions (init-clojure-try-extensions extension))
                 (test-file-names (->> extensions (--map (concat file-name-base "." it)))))
            (-if-let (test-file-name (->> test-file-names (-first #'f-exists?)))
                (find-file test-file-name)
              (user-error "Jump to test failed"))))))))

(keymap-set clojure-mode-map "C-x p t" #'init-clojure-test-jump)

(init-leader-set clojure-mode-map
  "p t" #'init-clojure-test-jump)



;;; flymake

(defvar init-clojure-kondo-program "clj-kondo")

(defun init-clojure-kondo-find-executable (_buffer)
  "Find kondo executable."
  (executable-find init-clojure-kondo-program))

(defun init-clojure-kondo-make-command (buffer)
  "Make kondo command for BUFFER."
  (let ((buffer-file-name (buffer-file-name buffer))
        (lang (if (not buffer-file-name)
                  "clj"
                (file-name-extension buffer-file-name))))
    `(,init-clojure-kondo-program
      "--lint" "-"
      "--lang" ,lang
      ,@(when buffer-file-name
          `("--filename" ,buffer-file-name)))))

(defconst init-clojure-kondo-diag-regexp
  "^.+:\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\([[:alpha:]]+\\): \\(.+\\)$")

(defvar init-clojure-kondo-type-alist
  '(("error" . :error) ("warning" . :warning)))

(defun init-clojure-kondo-make-diag (buffer)
  "Make Flymake diag for BUFFER from `match-string'."
  (let* ((row (string-to-number (match-string 1)))
         (col (string-to-number (match-string 2)))
         (type (-> (match-string 3) (assoc init-clojure-kondo-type-alist) (cdr)))
         (msg (match-string 4))
         (region (flymake-diag-region buffer row col)))
    (flymake-make-diagnostic
     buffer (car region) (cdr region) (or type :none) msg)))

(defun init-clojure-kondo-make-diags (buffer)
  "Make Flymake diags for BUFFER."
  (let (diags)
    (while (search-forward-regexp init-clojure-kondo-diag-regexp nil t)
      (push (init-clojure-kondo-make-diag buffer) diags))
    (nreverse diags)))

(defun init-clojure-kondo-report-diags (buffer proc-buffer report-fn)
  "Report Flymake diags in PROC-BUFFER for BUFFER.
REPORT-FN see `init-flymake-make-proc'."
  (funcall report-fn
           (with-current-buffer proc-buffer
             (widen)
             (goto-char (point-min))
             (init-clojure-kondo-make-diags buffer))))

(defun init-clojure-set-kondo ()
  "Set kondo Flymake backend."
  (setq-local init-flymake-find-executable-function #'init-clojure-kondo-find-executable)
  (setq-local init-flymake-make-command-function #'init-clojure-kondo-make-command)
  (setq-local init-flymake-report-diags-function #'init-clojure-kondo-report-diags)
  (add-hook 'flymake-diagnostic-functions #'init-flymake-backend nil t))

(add-hook 'clojure-mode-hook #'init-clojure-set-kondo)
(add-hook 'clojure-mode-hook #'flymake-mode)



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

(defun init-cider-repl-set-xref ()
  "Set Xref backend for Cider REPL."
  (add-hook 'xref-backend-functions #'cider--xref-backend nil t))

(add-hook 'cider-repl-mode-hook #'init-cider-repl-set-xref)

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
