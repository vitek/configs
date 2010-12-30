GCONFTOOL = gconftool-2

install: install-git install-hg install-emacs install-gconf

install-git:
	install -m 0644 git/gitconfig ~/.gitconfig
	install -m 0644 git/gitignore ~/.gitignore

install-hg:
	install -m 0644 hgrc ~/.hgrc

install-emacs:
	install -m 0644 emacs/emacs.el ~/.emacs

install-screen:
	install -m 0644 screenrc ~/.screenrc

install-gconf:
	$(GCONFTOOL) --set /desktop/gnome/interface/cursor_blink --type boolean 0
	$(GCONFTOOL) --set /apps/nautilus/preferences/show_desktop --type boolean 0
