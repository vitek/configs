GCONFTOOL = gconftool-2

install: install-git install-hg install-bzr install-emacs install-vim install-misc install-gconf

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
	install -d ~/.emacs.d/site-lisp
	install -m 0644 emacs/site-lisp/cython-mode.el ~/.emacs.d/site-lisp

install-vim:
	install -m 0644 vim/vimrc ~/.vimrc

install-misc:
	install -m 0644 screenrc ~/.screenrc
	install -m 0644 pylintrc ~/.pylintrc

install-gconf:
	$(GCONFTOOL) --set /desktop/gnome/interface/cursor_blink --type boolean 0
	$(GCONFTOOL) --set /apps/nautilus/preferences/show_desktop --type boolean 0
