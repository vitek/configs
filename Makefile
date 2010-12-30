GCONFTOOL = gconftool-2

install: install-git install-hg install-emacs install-gconf

install-git:
	install git/gitconfig ~/.gitconfig
	install git/gitignore ~/.gitignore

install-hg:
	install hgrc ~/.hgrc

install-emacs:
	install emacs/emacs.el ~/.emacs

install-screen:
	install screenrc ~/.screenrc

install-gconf:
	$(GCONFTOOL) --set /desktop/gnome/interface/cursor_blink --type boolean 0
	$(GCONFTOOL) --set /apps/nautilus/preferences/show_desktop --type boolean 0
