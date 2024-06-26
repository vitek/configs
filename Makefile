include Makefile.common

GCONFTOOL = gconftool-2

I3_DESTDIR = $(CONFIGDIR)/i3

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
	bin/terminal				\
	bin/ssh-auth-sock-watcher		\
	bin/run-emacsclient			\
	bin/myemacs                             \
	bin/xkb-layout				\
	bin/xml-pp

SYSTEMD_UNITS = systemd/xkb-layout.service

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

install-emacs: install-xresources
	$(MAKE) -C emacs install-emacs

install-emacs-norefresh: export EMACS_PACKAGES_REFRESH_SKIP=1
install-emacs-norefresh: install-emacs

install-vim:
	$(INSTALL) -m 0644 vim/vimrc $(DESTDIR)/.vimrc

install-misc: install-screen install-tmux
	$(INSTALL) -m 0644 pylintrc $(DESTDIR)/.pylintrc
	$(INSTALL) -d $(CONFIGDIR)
	$(INSTALL) -m 0644 flake8 $(CONFIGDIR)/flake8

install-screen:
	$(INSTALL) -m 0644 screenrc $(DESTDIR)/.screenrc

install-tmux:
	$(INSTALL) -m 0644 tmux.conf $(DESTDIR)/.tmux.conf

install-gconf:
	$(GCONFTOOL) --set /desktop/gnome/interface/cursor_blink --type boolean 0
	$(GCONFTOOL) --set /apps/nautilus/preferences/show_desktop --type boolean 0
	$(GCONFTOOL) --set /apps/gnome-terminal/profiles/Default/scrollbar_position --type string hidden

install-awesome: install-x11-utils
	$(INSTALL) -d $(CONFIGDIR)/awesome
	$(INSTALL) -m 0644 $(AWESOME_FILES) $(CONFIGDIR)/awesome/
	$(INSTALL) -m 0644 awesome/gnomerc $(DESTDIR)/.gnomerc
	$(INSTALL) -d $(CONFIGDIR)/awesome/apw
	$(INSTALL) -m 0644 awesome/apw/widget.lua awesome/apw/pulseaudio.lua \
		$(CONFIGDIR)/awesome/apw
	$(INSTALL) -d $(CONFIGDIR)/awesome/scripts
	@if [ -e $(CONFIGDIR)/awesome/scripts/start.sh ]; then	\
		echo "Awesome startup script already exists";		\
	else								\
		echo "Installing awesom startup script template";	\
		$(INSTALL) -m 0755 awesome/scripts/start.sh		\
			$(CONFIGDIR)/awesome/scripts/start.sh;	\
	fi

i3-reload: install-i3
	i3-msg reload

install-i3: install-xob install-i3status install-polybar install-x11-utils install-picom install-desktop-init
	$(INSTALL) -d $(I3_DESTDIR)
	$(INSTALL) -d $(I3_DESTDIR)/config.d
	$(INSTALL) -d $(I3_DESTDIR)/hostname.d
	$(INSTALL) -m 0644 i3/config $(I3_DESTDIR)
	$(INSTALL) -m 0644 i3/hostname.d/*.conf $(I3_DESTDIR)/hostname.d/
	$(INSTALL) -m 0644 i3/config.d/*.conf $(I3_DESTDIR)/config.d/
	$(INSTALL) -m 0755 i3/xkb-layout.py $(BINDIR)/i3-xkb-layout
	$(INSTALL) -m 0755 i3/i3-switch-layout $(BINDIR)/
	$(INSTALL) -m 0755 i3/i3-lock $(BINDIR)/

install-i3status:
	$(INSTALL) -d $(CONFIGDIR)/i3status
	$(INSTALL) -m 0644 i3/status/config $(CONFIGDIR)/i3status/

install-picom:
	$(INSTALL) -d $(CONFIGDIR)/
	$(INSTALL) -m 0644 picom.conf $(CONFIGDIR)/picom.conf

install-polybar:
	$(INSTALL) -d $(CONFIGDIR)/polybar
	$(INSTALL) -m 0644 polybar.ini $(CONFIGDIR)/polybar/config.ini

install-sway: install-desktop-init
	$(MAKE) -C wayland install-sway

install-desktop-init:
	$(INSTALL) -d $(BINDIR)/
	$(INSTALL) -m 0755 bin/desktop-init.sh $(BINDIR)/

install-xob:
	$(INSTALL) -d $(SYSTEMD_USER_PATH)
	$(INSTALL) -m 0644 xob.socket xob.service \
			$(SYSTEMD_USER_PATH)

reinstall-wob: install-wob
	systemctl --user daemon-reload
	systemctl --user restart wob.service

install-qt:
	$(INSTALL) -d $(CONFIGDIR)/qt5ct
	$(INSTALL) -m 0644 qt/qt5ct.conf $(CONFIGDIR)/qt5ct/
	$(INSTALL) -d $(DESTDIR)/.icons/default/
	$(INSTALL) -m 0644 icons/index.theme $(DESTDIR)/.icons/default/

restart-awesome: install-awesome
	awesome -k
	awesome --replace

install-compton:
	$(INSTALL) -d $(CONFIGDIR)
	$(INSTALL) -m 0644 awesome/compton.conf $(CONFIGDIR)/

install-x11-utils:
	$(INSTALL) -d $(BINDIR)
	$(INSTALL) -m 0644 xbindkeysrc $(DESTDIR)/.xbindkeysrc
	$(INSTALL) -m 0755 bin/scrot-area $(BINDIR)
	$(INSTALL) -m 0755 bin/zoom-mute $(BINDIR)
	$(INSTALL) -m 0755 bin/x11-display-ctl $(BINDIR)

install-bashrc:
	$(INSTALL) -m 0644 inputrc $(DESTDIR)/.inputrc
	$(INSTALL) -m 0644 profile $(DESTDIR)/.profile
	$(INSTALL) -m 0644 bashrc $(DESTDIR)/.bashrc
	$(INSTALL) -m 0644 bashrc_vterm $(DESTDIR)/.bashrc_vterm
	$(INSTALL) -d $(DESTDIR)/.bash.d
	$(INSTALL) -m 0644 bash.d/*.bash $(DESTDIR)/.bash.d

install-bin:
	$(INSTALL) -d $(BINDIR)
	$(INSTALL) -m 0755 $(BIN_FILES) $(BINDIR)/

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

install-ya: install-arc

install-arc: ya/arc ya/arc-bash-completion
	$(INSTALL) -d $(BINDIR)
	$(INSTALL) -m 0755 ya/arc $(BINDIR)
	$(INSTALL) -d $(SYSTEMD_USER_PATH)
	$(INSTALL) -m 0644 ya/arc.service $(SYSTEMD_USER_PATH)
	$(INSTALL) -d $(DESTDIR)/.local/share/bash-completion/completions/
	$(INSTALL) -m 0644 ya/arc-bash-completion $(DESTDIR)/.local/share/bash-completion/completions/arc
	$(INSTALL) -d $(DESTDIR)/.bash.d
	$(INSTALL) -m 0644 ya/arc-prompt.sh $(DESTDIR)/.bash.d/00-arc-prompt.sh

ya/arc:
	wget https://nda.ya.ru/t/LWWdfZJB52kkx7 -O $@.tmp
	chmod +x $@.tmp
	mv $@.tmp $@

ya/arc-bash-completion: ya/arc
	ya/arc completion bash > $@.tmp
	mv $@.tmp $@

clean-arc:
	rm -f ya/arc ya/arc-bash-completion

install-schroot:
	$(INSTALL) -d $(BINDIR)
	$(INSTALL) -m 0755 ya/schroot-session-helper $(BINDIR)/
	$(INSTALL) -m 0755 ya/schroot-session-end $(BINDIR)/
	ln -sf schroot-session-helper $(BINDIR)/bionic
	ln -sf schroot-session-helper $(BINDIR)/xenial
	ln -sf schroot-session-helper $(BINDIR)/focal
	sudo $(INSTALL) -m 0755 ya/schroot-mount.sh /etc/schroot/setup.d/99mount-home

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

clean: clean-arc
	$(MAKE) -C wayland clean
