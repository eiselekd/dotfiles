all:
	@echo "make monad  : install gnome + xmonad"
	@echo "make bashrc : install bashrc_extra"
	@echo "make claude-deepseek : install claude-deepseek script and proxy"
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


claude-deepseek:
	mkdir -p $(HOME)/.local/bin
	ln -sf $(CURDIR)/bin/claude-deepseek.sh $(HOME)/.local/bin/claude-deepseek
	chmod +x $(CURDIR)/bin/claude-deepseek.sh

prepare:
	sudo apt-get install xsel xdotool
