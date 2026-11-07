EMACS ?= emacs
BATCH = $(EMACS) -Q --batch --eval '(package-initialize)' -L lisp

.PHONY: setup
setup:
	cd .. && echo '(load-file (expand-file-name "emacs-init/init.el" user-emacs-directory))' > init.el
	cd .. && mkdir -p save lock backup

.PHONY: compile
compile:
	$(BATCH) --eval '(byte-recompile-directory "lisp" 0)'

.PHONY: test
test:
	$(BATCH) $(addprefix -l ,$(wildcard lisp/*-tests.el)) -f ert-run-tests-batch-and-exit < /dev/null

.PHONY: clean
clean:
	rm -f lisp/*.elc
