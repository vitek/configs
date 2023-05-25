include ../Makefile.common

install:
	$(INSTALL) -m 0655 mtp.service /lib/systemd/system/mtp.service
	$(INSTALL) -m 0655 99-music-player.rules /etc/udev/rules.d/99-music-player.rules
	udevadm control --reload
	systemctl daemon-reload
