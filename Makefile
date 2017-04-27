all:
	@echo "make monad  : install gnome + xmonad"
	@echo "make bashrc : install bashrc_extra"

monad:
	cp gnome/gnome-session-xmonad /usr/bin/
	cp gnome/gnome-xmonad.session /usr/share/gnome-session/sessions/
	cp gnome/gnome-xmonad.desktop /usr/share/xsessions/
	ln -sf $(CURDIR)/.xmonad/xmobar.hs $(HOME)/.xmobarrc

bashrc:
	echo 'if [ -f ~/git/dotfiles/.bashrc_extra ]; then' >> ~/.bashrc
	echo '  . ~/git/dotfiles/.bashrc_extra'             >> ~/.bashrc
	echo 'fi'                                           >> ~/.bashrc
