all:
	@echo "make monad : install gnome + xmonad"

monad:
	cp gnome/gnome-session-xmonad /usr/bin/
	cp gnome/gnome-xmonad.session /usr/share/gnome-session/sessions/
	cp gnome/gnome-xmonad.desktop /usr/share/xsessions/

