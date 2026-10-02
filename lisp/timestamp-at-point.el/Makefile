EMACS ?= emacs

.PHONY: compile clean

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile timestamp-at-point.el

clean:
	rm -f *.elc
