.PHONY: all
all: setup install compile

.PHONY: setup
setup:
	cp misc/init.el ..
	cd .. && mkdir -p save lock backup

.PHONY: install
install:
	emacs --script misc/init-install.el

.PHONY: compile
compile:
	emacs --script misc/init-compile.el

.PHONY: clean
clean:
	rm -f lisp/*.elc
