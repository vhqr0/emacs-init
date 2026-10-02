EMACS ?= emacs

.PHONY: compile clean

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile rg-dwim.el

clean:
	rm -f *.elc
