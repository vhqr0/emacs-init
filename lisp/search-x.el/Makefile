EMACS ?= emacs

.PHONY: compile clean

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile search-x.el

clean:
	rm -f *.elc
