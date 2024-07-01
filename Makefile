.PHONY: setup
setup:
	cp misc/init.el ..
	cp misc/early-init.el ..
	cd .. && mkdir -p save lock backup
	emacs --script misc/setup.el

.PHONY: clean
clean:
	rm -f lisp/*.elc site/*.elc
