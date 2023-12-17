.PHONY: setup
setup:
	cp misc/init.el ..
	cp misc/early-init.el ..
	cd .. && mkdir -p auto-save lock backup trash
	emacs --script misc/setup.el

.PHONY: clean
clean:
	rm -f lisp/*.elc site/*.elc site/site-autoloads.el
