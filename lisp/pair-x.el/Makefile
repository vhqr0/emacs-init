EMACS ?= emacs
BATCH = $(EMACS) -Q --batch -L .

.PHONY: test compile clean

test:
	$(BATCH) -l pair-x-tests.el -f ert-run-tests-batch-and-exit < /dev/null

compile:
	$(BATCH) -f batch-byte-compile pair-x.el

clean:
	rm -f *.elc
