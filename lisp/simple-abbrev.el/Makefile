EMACS ?= emacs

.PHONY: compile clean

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile simple-abbrev.el

clean:
	rm -f *.elc
