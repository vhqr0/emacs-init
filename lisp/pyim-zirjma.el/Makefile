EMACS ?= emacs

.PHONY: compile clean

compile:
	$(EMACS) -Q --batch --eval '(package-initialize)' -L . -f batch-byte-compile pyim-zirjma.el

clean:
	rm -f *.elc
