;;; init-clojure.el --- Init Clojure -*- lexical-binding: t; no-native-compile: t -*-

;;; Commentary:
;; Various init configuration for Clojure language.

;;; Code:

(require 'init-emacs)



;;; major mode

(require 'clojure-mode)

(keymap-set clojure-refactor-map ":" #'clojure-toggle-keyword-string)
(keymap-set clojure-refactor-map "SPC" #'clojure-align)

(evil-define-key 'normal clojure-mode-map
  "gr" clojure-refactor-map)

(defvar init-clojure-modes
  '(clojurec-mode clojure-mode clojurescript-mode))

(add-hook 'clojure-mode-hook #'init-elisp-set-outline)

(dolist (mode init-clojure-modes)
  (add-to-list 'init-evil-eval-function-alist `(,mode . cider-eval-region)))

;;;; test jump

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



;;; flymake

;; Ref: flymake-kondor https://github.com/turbo-cafe/flymake-kondor

(defvar init-clojure-kondo-buffer-name "*flymake-clj-kondo*")

(defvar init-clojure-kondo-program "clj-kondo")

(defconst init-clojure-kondo-diag-regexp
  "^.+:\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\([[:alpha:]]+\\): \\(.+\\)$")

(defvar-local init-clojure-kondo-proc nil)

(defun init-clojure-build-kondo-command (buffer)
  "Build clj-kondo command of BUFFER for Flymake."
  (let ((buffer-file-name (buffer-file-name buffer))
        (lang (if (not buffer-file-name)
                  "clj"
                (file-name-extension buffer-file-name))))
    `(,init-clojure-kondo-program
      "--lint" "-"
      "--lang" ,lang
      ,@(when buffer-file-name
          `("--filename" ,buffer-file-name)))))

(defun init-clojure-get-kondo-error-type (type-string)
  "Get clj-kondo error type for Flymake from TYPE-STRING."
  (cond ((string= type-string "error") :error)
        ((string= type-string "warning") :warning)
        (t :none)))

(defun init-clojure-make-kondo-diag (buffer)
  "Build clj-kondo diag of BUFFER for Flymake."
  (let* ((row (string-to-number (match-string 1)))
         (col (string-to-number (match-string 2)))
         (type (init-clojure-get-kondo-error-type (match-string 3)))
         (msg (match-string 4))
         (region (flymake-diag-region buffer row col)))
    (flymake-make-diagnostic buffer (car region) (cdr region) type msg)))

(defun init-clojure-make-kondo-diag-list (buffer)
  "Build clj-kondo diag list of BUFFER for flymake."
  (let (diags)
    (while (search-forward-regexp init-clojure-kondo-diag-regexp nil t)
      (push (init-clojure-make-kondo-diag buffer) diags))
    (nreverse diags)))

(defun init-clojure-kondo-sentinel (proc buffer report-fn)
  "The clj-kondo sentinel of BUFFER and PROC for Flymake with REPORT-FN."
  (when (memq (process-status proc) '(exit signal))
    (let ((proc-buffer (process-buffer proc)))
      (unwind-protect
          (if (eq proc (buffer-local-value 'init-clojure-kondo-proc buffer))
              (with-current-buffer proc-buffer
                (goto-char (point-min))
                (funcall report-fn (init-clojure-make-kondo-diag-list buffer)))
            (flymake-log :warning "Canceling obsolete checker %s" proc))
        (kill-buffer proc-buffer)))))

(defun init-clojure-make-kondo-sentinel (buffer report-fn)
  "Build the clj-kondo process sentinel of BUFFER for Flymake with REPORT-FN."
  (lambda (proc _event)
    (init-clojure-kondo-sentinel proc buffer report-fn)))

(defun init-clojure-make-kondo-proc (buffer report-fn)
  "Build the clj-kondo process of BUFFER for Flymake with REPORT-FN."
  (make-process
   :name init-clojure-kondo-buffer-name
   :noquery t
   :connection-type 'pipe
   :buffer (generate-new-buffer init-clojure-kondo-buffer-name)
   :command (init-clojure-build-kondo-command buffer)
   :sentinel (init-clojure-make-kondo-sentinel buffer report-fn)))

(defun init-clojure-flymake-kondo-backend (report-fn &rest _args)
  "Build the Flymake backend for clj-kondo with REPORT-FN."
  (unless (executable-find init-clojure-kondo-program)
    (user-error "Executable kondo not found"))
  (save-restriction
    (widen)
    (let ((proc (init-clojure-make-kondo-proc (current-buffer) report-fn)))
      (when (process-live-p init-clojure-kondo-proc)
        (kill-process init-clojure-kondo-proc))
      (setq init-clojure-kondo-proc proc)
      (process-send-region proc (point-min) (point-max))
      (process-send-eof proc))))

(defun init-clojure-set-flymake ()
  "Set Flymake backend."
  (add-hook 'flymake-diagnostic-functions #'init-clojure-flymake-kondo-backend nil t))

(add-hook 'clojure-mode-hook #'init-clojure-set-flymake)
(add-hook 'clojure-mode-hook #'flymake-mode)



;;; cider

(require 'cider)
(require 'cider-format)
(require 'cider-macroexpansion)

(add-hook 'cider-repl-mode-hook #'init-set-corfu-auto)

(setq cider-mode-line '(:eval (format " Cider[%s]" (cider--modeline-info))))

(dolist (map (list cider-mode-map cider-repl-mode-map))
  (define-key map [remap evil-lookup] #'cider-doc)
  (define-key map [remap evil-goto-definition] #'cider-find-var))

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
