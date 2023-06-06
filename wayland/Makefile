include ../Makefile.common

SWAY_BIN_FILES =				\
	grimshot				\
	launcher-menu				\
	sway-emacs				\
	sway-idle.sh				\
	sway-lock.sh				\
	sway-notify-ready			\
	sway-quake				\
	sway-session.sh				\
	sway-switch-layout			\
	sway-user-service			\
	wait-sni-ready				\
	wf-recorder.sh

SWAY_SYSTEMD_FILES =				\
	systemd/gnome-keyring-daemon.service	\
	systemd/sway-autostart.target		\
	systemd/sway-kbd.service		\
	systemd/sway-nm-applet.service		\
	systemd/sway.service			\
	systemd/sway-session-pre.target		\
	systemd/sway-session.target		\
	systemd/sway-xdg-autostart.target	\
	systemd/sway-wait-sni-ready.service	\
	systemd/wayland-session-pre.target	\
	systemd/wayland-session.target

install-sway: install-waybar install-wob install-desktopctl install-dunst install-sway-session install-sway-dpms
	$(INSTALL) -d $(CONFIGDIR)/sway
	$(INSTALL) -d $(CONFIGDIR)/sway/config.d
	$(INSTALL) -d $(CONFIGDIR)/sway/hostname.d
	$(INSTALL) -m 0644 sway/config $(CONFIGDIR)/sway/
	$(INSTALL) -m 0644 sway/hostname.d/*.conf $(CONFIGDIR)/sway/hostname.d/
	$(INSTALL) -m 0644 sway/config.d/*.conf $(CONFIGDIR)/sway/config.d/
	$(INSTALL) -d $(BINDIR)
	$(INSTALL) -m 0755 $(SWAY_BIN_FILES) $(BINDIR)

install-sway-dpms:
	$(INSTALL) -d $(BINDIR)
	$(INSTALL) -m 0755 sway-dpms $(BINDIR)/sway-dpms

install-sway-session: $(SWAY_SYSTEMD_FILES)
	$(INSTALL) -d $(SYSTEMD_USER_PATH)
	$(INSTALL) -m 0644 $(SWAY_SYSTEMD_FILES) $(SYSTEMD_USER_PATH)
	systemctl --user daemon-reload

install-waybar:
	$(INSTALL) -d $(CONFIGDIR)/waybar
	$(INSTALL) -m 0644 waybar/config $(CONFIGDIR)/waybar/
	$(INSTALL) -m 0644 waybar/style.css $(CONFIGDIR)/waybar/

install-wob:
	$(INSTALL) -d $(SYSTEMD_USER_PATH)
	$(INSTALL) -m 0644 wob/wob.socket wob/wob.service \
			$(SYSTEMD_USER_PATH)
	$(INSTALL) -d $(CONFIGDIR)/wob
	$(INSTALL) -m 0644 wob/wob.ini $(CONFIGDIR)/wob

install-desktopctl:
	$(INSTALL) -d $(BINDIR)
	$(INSTALL) -m 0755 desktopctl $(BINDIR)

install-dunst:
	$(INSTALL) -d $(CONFIGDIR)/dunst/
	$(INSTALL) -m 0644 dunstrc $(CONFIGDIR)/dunst/dunstrc

clean:
	rm -f systemd/sway-lock.service
