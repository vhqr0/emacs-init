.PHONY: setup
setup:
	cd .. && echo '(load-file (expand-file-name "emacs-init/init.el" user-emacs-directory))' > init.el
	cd .. && mkdir -p save lock backup

.PHONY: compile
compile:
	$(MAKE) -C vim.el compile
