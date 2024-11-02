.PHONY: all
all: setup install compile

.PHONY: setup
setup:
	cp misc/init.el ..
	cp misc/early-init.el ..
	cd .. && mkdir -p save lock backup

.PHONY: install
install:
	emacs --script misc/install.el

.PHONY: compile
compile:
	emacs --script misc/compile.el

.PHONY: clean
clean:
	rm -f lisp/*.elc
