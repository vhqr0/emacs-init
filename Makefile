.PHONY: setup
setup:
	cd .. && echo '(load-file (expand-file-name "emacs-init/init.el" user-emacs-directory))' > init.el
	cd .. && mkdir -p save lock backup

.PHONY: compile
compile:
	$(MAKE) -C lisp/vim.el compile
	$(MAKE) -C lisp/project-test-jump.el compile
	$(MAKE) -C lisp/flymake-x.el compile
	$(MAKE) -C lisp/eshell-dwim.el compile
	$(MAKE) -C lisp/simple-abbrev.el compile
	$(MAKE) -C lisp/macrostep-cider.el compile
	$(MAKE) -C lisp/pyim-zirjma.el compile
	$(MAKE) -C lisp/rg-dwim.el compile
	$(MAKE) -C lisp/timestamp-at-point.el compile
	$(MAKE) -C lisp/header-line-x.el compile
	$(MAKE) -C lisp/outline-x.el compile
	$(MAKE) -C lisp/markdown-x.el compile
