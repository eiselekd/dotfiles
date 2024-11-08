all:
	@echo "make monad  : install gnome + xmonad"
	@echo "make bashrc : install bashrc_extra"
	@echo "nmcli device wifi rescan"
	@echo "nmcli device wifi list"
	@echo "nmcli device wifi connect <SSID-Name> password <wireless-password>"


monad:
	cp gnome/gnome-session-xmonad /usr/bin/
	cp gnome/gnome-xmonad.session /usr/share/gnome-session/sessions/
	cp gnome/gnome-xmonad.desktop /usr/share/xsessions/
	cp gnome/xmonad.desktop       /usr/share/xsessions/
	ln -sf $(CURDIR)/.xmonad/xmobar.hs $(HOME)/.xmobarrc

bashrc:
	echo 'if [ -f ~/git/dotfiles/.bashrc_extra ]; then' >> ~/.bashrc
	echo '  . ~/git/dotfiles/.bashrc_extra'             >> ~/.bashrc
	echo 'fi'                                           >> ~/.bashrc


prepare:
	sudo apt-get install xsel xdotool
