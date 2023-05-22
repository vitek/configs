include ../Makefile.common

SWAY_BIN_FILES =				\
	grimshot				\
	sway-session.sh				\
	wait-sni-ready				\
	sway-dpms				\
	sway-switch-layout			\
	sway-lock.sh				\
	sway-idle.sh				\
	sway-quake				\
	sway-notify-ready			\
	sway-user-service			\
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

install-sway: install-waybar install-wob install-desktopctl install-dunst install-sway-session
	$(INSTALL) -d $(DESTDIR)/.config/sway
	$(INSTALL) -d $(DESTDIR)/.config/sway/config.d
	$(INSTALL) -d $(DESTDIR)/.config/sway/hostname.d
	$(INSTALL) -m 0644 sway/config $(DESTDIR)/.config/sway/
	$(INSTALL) -m 0644 sway/hostname.d/*.conf $(DESTDIR)/.config/sway/hostname.d/
	$(INSTALL) -m 0644 sway/config.d/*.conf $(DESTDIR)/.config/sway/config.d/
	$(INSTALL) -m 0755 $(SWAY_BIN_FILES) $(DESTDIR)/bin/

install-sway-session: $(SWAY_SYSTEMD_FILES)
	$(INSTALL) -d $(SYSTEMD_USER_PATH)
	$(INSTALL) -m 0644 $(SWAY_SYSTEMD_FILES) $(SYSTEMD_USER_PATH)
	systemctl --user daemon-reload

install-waybar:
	$(INSTALL) -d $(DESTDIR)/.config/waybar
	$(INSTALL) -m 0644 waybar/config $(DESTDIR)/.config/waybar/
	$(INSTALL) -m 0644 waybar/style.css $(DESTDIR)/.config/waybar/

install-wob:
	$(INSTALL) -d $(SYSTEMD_USER_PATH)
	$(INSTALL) -m 0644 wob/wob.socket wob/wob.service \
			$(SYSTEMD_USER_PATH)
	$(INSTALL) -d $(DESTDIR)/.config/wob
	$(INSTALL) -m 0644 wob/wob.ini $(DESTDIR)/.config/wob

install-desktopctl:
	$(INSTALL) -d $(DESTDIR)/bin
	$(INSTALL) -m 0755 desktopctl $(DESTDIR)/bin

install-dunst:
	$(INSTALL) -d $(DESTDIR)/.config/dunst/
	$(INSTALL) -m 0644 dunstrc $(DESTDIR)/.config/dunst/dunstrc

clean:
	rm -f systemd/sway-lock.service
