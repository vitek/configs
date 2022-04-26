GCONFTOOL = gconftool-2
INSTALL   = install
PYTHON3   = $(firstword $(shell which python3.9 python3.8 python3.7 python3))
EMACS     = emacs

DESTDIR   = $(HOME)

AWESOME_FILES =					\
	awesome/rc.lua				\
	awesome/mywibar.lua			\
	awesome/battery.lua			\
	awesome/brightness.lua			\
	awesome/keyboard_layout.lua		\
	awesome/utils.lua			\
	awesome/quake.lua			\
	awesome/xrandr.lua

BIN_FILES =					\
	bin/authinfo-tool			\
	bin/brightnessctl.sh			\
	bin/captive.sh				\
	bin/clangd-chooser			\
	bin/histogram				\
	bin/json-pp				\
	bin/xkb-layout				\
	bin/xml-pp

SWAY_BIN_FILES =				\
	wayland/grimshot			\
	wayland/sway-switch-layout		\
	wayland/sway-lock.sh			\
	wayland/sway-quake

SYSTEMD_UNITS = systemd/xkb-layout.service
SYSTEMD_USER_PATH = $(DESTDIR)/.config/systemd/user/

default:

install: install-vc install-editor install-misc

install-vc: install-git install-hg install-bzr
install-editor: install-emacs install-vim

install-all: 					\
	install-awesome				\
	install-bashrc				\
	install-bin				\
	install-bzr				\
	install-dconf				\
	install-editor				\
	install-emacs				\
	install-git				\
	install-hg				\
	install-misc				\
	install-sway				\
	install-vc				\
	install-vim				\
	install-xresources

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

install-xresources:
	$(INSTALL) -m 0644 Xresources $(DESTDIR)/.Xresources

install-emacs-configs:
	$(INSTALL) -d $(DESTDIR)/.emacs.d/site-lisp
	$(INSTALL) -d $(DESTDIR)/.emacs.d/scripts
	rm -f  $(DESTDIR)/.emacs
	$(INSTALL) -m 0644 emacs/init.el $(DESTDIR)/.emacs.d/init.el
	$(INSTALL) -m 0644 emacs/early-init.el $(DESTDIR)/.emacs.d/early-init.el
	$(INSTALL) -m 0644 emacs/site-lisp/git-grep.el $(DESTDIR)/.emacs.d/site-lisp
	$(INSTALL) -m 0644 emacs/site-lisp/google-c-style.el $(DESTDIR)/.emacs.d/site-lisp
	$(INSTALL) -m 0644 emacs/site-lisp/mc-move.el $(DESTDIR)/.emacs.d/site-lisp
	$(INSTALL) -m 0644 emacs/site-lisp/tools.el $(DESTDIR)/.emacs.d/site-lisp
	$(INSTALL) -m 0644 emacs/site-lisp/my.el $(DESTDIR)/.emacs.d/site-lisp
	$(INSTALL) -m 0644 emacs/site-lisp/manual-indent.el $(DESTDIR)/.emacs.d/site-lisp
	$(INSTALL) -m 0644 emacs/site-lisp/zoom-frm.el $(DESTDIR)/.emacs.d/site-lisp
	$(INSTALL) -m 0644 emacs/site-lisp/frame-fns.el $(DESTDIR)/.emacs.d/site-lisp
	$(INSTALL) -m 0644 emacs/site-lisp/frame-cmds.el $(DESTDIR)/.emacs.d/site-lisp
	$(INSTALL) -m 0644 emacs/scripts/pyimpsort.py $(DESTDIR)/.emacs.d/scripts/pyimpsort.py

install-emacs: install-xresources install-emacs-configs
	$(EMACS) --batch --script emacs/setup.el $(DESTDIR)/.emacs.d

install-emacs-norefresh: export EMACS_PACKAGES_REFRESH_SKIP=1
install-emacs-norefresh: install-emacs

test-emacs:
	cd emacs/site-lisp && $(EMACS) -batch -f package-initialize -L . -f buttercup-run-discover

install-vim:
	$(INSTALL) -m 0644 vim/vimrc $(DESTDIR)/.vimrc

install-misc:
	$(INSTALL) -m 0644 screenrc $(DESTDIR)/.screenrc
	$(INSTALL) -m 0644 pylintrc $(DESTDIR)/.pylintrc
	$(INSTALL) -d $(DESTDIR)/.config
	$(INSTALL) -m 0644 flake8 $(DESTDIR)/.config/flake8

install-gconf:
	$(GCONFTOOL) --set /desktop/gnome/interface/cursor_blink --type boolean 0
	$(GCONFTOOL) --set /apps/nautilus/preferences/show_desktop --type boolean 0
	$(GCONFTOOL) --set /apps/gnome-terminal/profiles/Default/scrollbar_position --type string hidden

install-awesome: install-x11-utils
	$(INSTALL) -d $(DESTDIR)/.config/awesome
	$(INSTALL) -m 0644 $(AWESOME_FILES) $(DESTDIR)/.config/awesome/
	$(INSTALL) -m 0644 awesome/gnomerc $(DESTDIR)/.gnomerc
	$(INSTALL) -d $(DESTDIR)/.config/awesome/apw
	$(INSTALL) -m 0644 awesome/apw/widget.lua awesome/apw/pulseaudio.lua \
		$(DESTDIR)/.config/awesome/apw
	$(INSTALL) -d $(DESTDIR)/.config/awesome/scripts
	@if [ -e $(DESTDIR)/.config/awesome/scripts/start.sh ]; then	\
		echo "Awesome startup script already exists";		\
	else								\
		echo "Installing awesom startup script template";	\
		$(INSTALL) -m 0755 awesome/scripts/start.sh		\
			$(DESTDIR)/.config/awesome/scripts/start.sh;	\
	fi


install-waybar:
	$(INSTALL) -d $(DESTDIR)/.config/waybar
	$(INSTALL) -m 0644 wayland/waybar/config $(DESTDIR)/.config/waybar/
	$(INSTALL) -m 0644 wayland/waybar/style.css $(DESTDIR)/.config/waybar/

install-sway: install-waybar
	$(INSTALL) -d $(DESTDIR)/.config/sway
	$(INSTALL) -d $(DESTDIR)/.config/sway/config.d
	$(INSTALL) -d $(DESTDIR)/.config/sway/hostname.d
	$(INSTALL) -m 0644 wayland/sway/config $(DESTDIR)/.config/sway/
	$(INSTALL) -m 0644 wayland/sway/hostname.d/*.conf $(DESTDIR)/.config/sway/hostname.d/
	$(INSTALL) -m 0644 wayland/sway/config.d/*.conf $(DESTDIR)/.config/sway/config.d/
	$(INSTALL) -m 0755 $(SWAY_BIN_FILES) $(DESTDIR)/bin/

restart-awesome: install-awesome
	awesome -k
	awesome --replace

install-compton:
	$(INSTALL) -d $(DESTDIR)/.config
	$(INSTALL) -m 0644 awesome/compton.conf $(DESTDIR)/.config/

install-x11-utils:
	$(INSTALL) -m 0644 xbindkeysrc $(DESTDIR)/.xbindkeysrc

install-bashrc:
	$(INSTALL) -m 0644 inputrc $(DESTDIR)/.inputrc
	$(INSTALL) -m 0644 profile $(DESTDIR)/.profile
	$(INSTALL) -m 0644 bashrc $(DESTDIR)/.bashrc
	$(INSTALL) -m 0644 bashrc_vterm $(DESTDIR)/.bashrc_vterm

install-bin:
	$(INSTALL) -d $(DESTDIR)/bin
	$(INSTALL) -m 0755 $(BIN_FILES) $(DESTDIR)/bin/

install-ubuntu-extra:
	python installpkgs.py ubuntu-packages

install-sound-theme:
	$(INSTALL) -d $(DESTDIR)/.local/share/sounds/__custom/
	$(INSTALL) custom-sound-theme.theme $(DESTDIR)/.local/share/sounds/__custom/index.theme
	touch $(DESTDIR)/.local/share/sounds/__custom/screen-capture.disabled

install-dconf: install-sound-theme
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

install-logid:
	sudo cp logid.cfg /etc/logid.cfg

install-systemd-units: $(SYSTEMD_UNITS)
	$(INSTALL) -d $(SYSTEMD_USER_PATH)
	for unit in $(SYSTEMD_UNITS); do \
	  $(INSTALL) -m 0644 $$unit $(SYSTEMD_USER_PATH); \
	  systemctl --user enable $$(basename $$unit); \
	done

install-systemd: install-bin install-systemd-units

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

%.service: %.service.in
	$(PYTHON3) substitute.py -i $< -o $@ --\
		"HOME=$(DESTDIR)" "PYTHON3=$(PYTHON3)"
