GCONFTOOL = gconftool-2
INSTALL   = install

DESTDIR   = $(HOME)


default:

install: install-vc install-editor install-misc

install-vc: install-git install-hg install-bzr
install-editor: install-emacs install-vim


install-all: install-awesome			\
	install-bashrc				\
	install-bzr				\
	install-dconf				\
	install-editor				\
	install-emacs				\
	install-git				\
	install-hg				\
	install-misc				\
	install-vc				\
	install-vim

install-git:
	$(INSTALL) -m 0644 git/gitconfig $(DESTDIR)/.gitconfig
	$(INSTALL) -m 0644 git/gitignore $(DESTDIR)/.gitignore

install-hg:
	$(INSTALL) -m 0644 hgrc $(DESTDIR)/.hgrc

install-bzr:
	$(INSTALL) -d $(DESTDIR)/.bazaar
	$(INSTALL) -m 0644 bzr/bazaar.conf $(DESTDIR)/.bazaar/
	$(INSTALL) -m 0644 bzr/authentication.conf $(DESTDIR)/.bazaar/
	$(INSTALL) -m 0644 bzr/ignore $(DESTDIR)/.bazaar/

install-emacs:
	$(INSTALL) -d $(DESTDIR)/.emacs.d/site-lisp
	$(INSTALL) -m 0644 emacs/emacs.el $(DESTDIR)/.emacs
	$(INSTALL) -m 0644 emacs/site-lisp/cython-mode.el $(DESTDIR)/.emacs.d/site-lisp
	$(INSTALL) -m 0644 emacs/site-lisp/column-marker.el $(DESTDIR)/.emacs.d/site-lisp
	$(INSTALL) -m 0644 emacs/site-lisp/github-theme.el $(DESTDIR)/.emacs.d/site-lisp
	$(INSTALL) -m 0644 emacs/site-lisp/git-grep.el $(DESTDIR)/.emacs.d/site-lisp

install-vim:
	$(INSTALL) -m 0644 vim/vimrc $(DESTDIR)/.vimrc

install-misc:
	$(INSTALL) -m 0644 screenrc $(DESTDIR)/.screenrc
	$(INSTALL) -m 0644 pylintrc $(DESTDIR)/.pylintrc

install-gconf:
	$(GCONFTOOL) --set /desktop/gnome/interface/cursor_blink --type boolean 0
	$(GCONFTOOL) --set /apps/nautilus/preferences/show_desktop --type boolean 0
	$(GCONFTOOL) --set /apps/gnome-terminal/profiles/Default/scrollbar_position --type string hidden

install-awesome:
	$(INSTALL) -d $(DESTDIR)/.config/awesome
	$(INSTALL) -d $(DESTDIR)/.config/awesome/scripts
	$(INSTALL) -m 0644 awesome/rc.lua $(DESTDIR)/.config/awesome/rc.lua
	$(INSTALL) -m 0755 awesome/scripts/touchpad-init.sh \
		$(DESTDIR)/.config/awesome/scripts/touchpad-init.sh
	$(INSTALL) -m 0755 awesome/scripts/keyboard.sh \
		$(DESTDIR)/.config/awesome/scripts/keyboard.sh
	$(INSTALL) -m 0644 awesome/gnomerc $(DESTDIR)/.gnomerc

install-bashrc:
	$(INSTALL) -m 0644 profile $(DESTDIR)/.profile
	$(INSTALL) -m 0644 bashrc $(DESTDIR)/.bashrc

install-ubuntu-extra:
	python installpkgs.py ubuntu-packages

install-dconf:
	dconf load / < dconf-settings

install-macbook:
	sudo cp macbook/modprobe.d/macbook-fixes.conf /etc/modprobe.d/macbook-fixes.conf

install-awesome-session:
	sudo cp awesome/session/gnome-awesome.session \
		/usr/share/gnome-session/sessions/gnome-awesome.session
	sudo cp awesome/session/awesome.desktop \
		/usr/share/applications/awesome.desktop
	sudo cp awesome/session/gnome-awesome.desktop \
		/usr/share/xsessions/gnome-awesome.desktop


.PHONY: diff
diff:
	@rm -rf test-config
	@$(MAKE) install-all DESTDIR=test-config
	@cd test-config && find . -type f | while read fname; do \
		diff -urN $$fname ~/$$fname; \
	done
	@rm -rf test-config

TARBALL_TARGETS = \
	install-editor \
	install-vc \
	install-misc \
	install-bashrc

.PHONY: configs.tar.gz ssh-deploy
configs.tar.gz:
	rm -rf .tarball
	$(MAKE) $(TARBALL_TARGETS) DESTDIR=.tarball
	tar -zcf $@ -C .tarball .
	rm -rf .tarball

ssh-deploy: configs.tar.gz
	@if [ "x$(SSH_HOSTNAME)" = "x" ]; then \
		echo "Please set SSH_HOSTNAME variable"; \
		exit 1; \
	fi
	@for hostname in $(SSH_HOSTNAME); do \
		echo "Installing configs to $$hostname..."; \
		cat $< | ssh $$hostname tar -zxf - -C ~ || exit 1; \
	done
