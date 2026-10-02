EMACS ?= emacs

.PHONY: compile clean

compile:
	$(EMACS) -Q --batch --eval '(package-initialize)' -L . -f batch-byte-compile macrostep-cider.el

clean:
	rm -f *.elc
