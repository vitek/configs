install: install-git install-hg install-emacs

install-git:
	install git/gitconfig ~/.gitconfig
	install git/gitignore ~/.gitignore

install-hg:
	install hgrc ~/.hgrc

install-emacs:
	install emacs/emacs.el ~/.emacs
