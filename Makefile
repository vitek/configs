GCONFTOOL = gconftool-2

install: install-git install-hg install-bzr install-emacs install-vim install-gconf

install-git:
	install -m 0644 git/gitconfig ~/.gitconfig
	install -m 0644 git/gitignore ~/.gitignore

install-hg:
	install -m 0644 hgrc ~/.hgrc

install-bzr:
	install -d ~/.bazaar
	for i in authentication.conf bazaar.conf ignore; do \
	    install -m 0644 bzr/$$i ~/.bazaar/;                 \
	done;

install-emacs:
	install -m 0644 emacs/emacs.el ~/.emacs

install-vim:
	install -m 0644 vim/vimrc ~/.vimrc

install-screen:
	install -m 0644 screenrc ~/.screenrc

install-gconf:
	$(GCONFTOOL) --set /desktop/gnome/interface/cursor_blink --type boolean 0
	$(GCONFTOOL) --set /apps/nautilus/preferences/show_desktop --type boolean 0
