EMACS ?= emacs
BATCH = $(EMACS) -Q --batch --eval '(package-initialize)' -L .

.PHONY: test compile clean

test:
	$(BATCH) -l markdown-x-tests.el -f ert-run-tests-batch-and-exit < /dev/null

compile:
	$(BATCH) -f batch-byte-compile markdown-x.el

clean:
	rm -f *.elc
