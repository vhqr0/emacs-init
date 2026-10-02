EMACS ?= emacs

.PHONY: compile clean

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile simple-format.el

clean:
	rm -f *.elc
