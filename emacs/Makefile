include ../Makefile.common

EMACS          = emacs

DEST_EMACSDIR  = $(DESTDIR)/.emacs.d
DEST_SITE_LISP = $(DEST_EMACSDIR)/site-lisp
DEST_SCRIPTS   = $(DEST_EMACSDIR)/.emacs.d/scripts

EL_FILES =					\
	init.el					\
	early-init.el

SITE_LISP_FILES =				\
	site-lisp/frame-cmds.el			\
	site-lisp/frame-fns.el			\
	site-lisp/manual-indent.el		\
	site-lisp/mc-move.el			\
	site-lisp/my.el				\
	site-lisp/tools.el			\
	site-lisp/vitja-ivy.el			\
	site-lisp/vitja-vertico.el		\
	site-lisp/yaml-xref.el			\
	site-lisp/zoom-frm.el

SCRIPT_FILES =					\
	scripts/pyimpsort.py			\
	scripts/yaml-xref

install-emacs: install-emacs-configs
	$(EMACS) --batch --script setup.el $(DEST_EMACSDIR)

install-emacs-norefresh: export EMACS_PACKAGES_REFRESH_SKIP=1
install-emacs-norefresh: install-emacs

install-emacs-configs: install-emacs-el install-site-lisp install-scripts
	# Remove obsolete configs and direcrories
	rm -f  $(DESTDIR)/.emacs
	rm -f $(DEST_SITE_LISP)/git-grep.el* \
		$(DEST_SITE_LISP)/google-c-style.el*

install-emacs-el:
	$(INSTALL) -d $(DEST_EMACSDIR)
	$(INSTALL) -m 0644 $(EL_FILES) $(DEST_EMACSDIR)

install-site-lisp:
	$(INSTALL) -d $(DEST_SITE_LISP)
	$(INSTALL) -m 0644 $(SITE_LISP_FILES) $(DEST_SITE_LISP)

install-scripts:
	$(INSTALL) -d $(DEST_SCRIPTS)
	$(INSTALL) -m 0755 $(SCRIPT_FILES) $(DEST_SCRIPTS)

test-emacs:
	cd site-lisp && $(EMACS) -batch -f package-initialize -L . -f buttercup-run-discover

