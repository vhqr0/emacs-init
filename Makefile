els := $(shell emacs --script misc/init-lisp-files.el 2>&1)
elcs := $(patsubst %.el, %.elc, $(els))

.PHONY: all
all: site/site-autoloads.el $(elcs)

$(elcs): %.elc: %.el
	emacs --script misc/compile-files.el $<

site/site-autoloads.el: $(filter site/%.el, $(els))
	rm -f $@
	emacs --script misc/generate-loaddefs.el site $@

.PHONY: clean
clean:
	rm -f lisp/*.elc site/*.elc site/site-autoloads.el

.PHONY: elpa-install
elpa-install:
	emacs --script misc/init-elpa-install.el

.PHONY: init
init:
	cp misc/init.el ..
	cp misc/early-init.el ..
	cd .. && mkdir -p auto-save lock backup undo-tree trash
