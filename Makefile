GCONFTOOL = gconftool-2
INSTALL   = install

install: install-vc install-editor install-misc

install-vc: install-git install-hg install-bzr
install-editor: install-emacs install-vim

install-git:
	$(INSTALL) -m 0644 git/gitconfig ~/.gitconfig
	$(INSTALL) -m 0644 git/gitignore ~/.gitignore

install-hg:
	$(INSTALL) -m 0644 hgrc ~/.hgrc

install-bzr:
	$(INSTALL) -d ~/.bazaar
	$(INSTALL) -m 0644 bzr/bazaar.conf ~/.bazaar/
	$(INSTALL) -m 0644 bzr/authentication.conf ~/.bazaar/
	$(INSTALL) -m 0644 bzr/ignore ~/.bazaar/

install-emacs:
	$(INSTALL) -d ~/.emacs.d/site-lisp
	$(INSTALL) -m 0644 emacs/emacs.el ~/.emacs
	$(INSTALL) -m 0644 emacs/site-lisp/cython-mode.el ~/.emacs.d/site-lisp

install-vim:
	$(INSTALL) -m 0644 vim/vimrc ~/.vimrc

install-misc:
	$(INSTALL) -m 0644 screenrc ~/.screenrc
	$(INSTALL) -m 0644 pylintrc ~/.pylintrc

install-gconf:
	$(GCONFTOOL) --set /desktop/gnome/interface/cursor_blink --type boolean 0
	$(GCONFTOOL) --set /apps/nautilus/preferences/show_desktop --type boolean 0
