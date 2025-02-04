all:


pre:
	wget -qO- https://get.haskellstack.org/ | sh

prep:
	rm -rf xmonad-git xmonad-contrib-git xmobar-git
	git clone "https://github.com/xmonad/xmonad" xmonad-git
	git clone "https://github.com/xmonad/xmonad-contrib" xmonad-contrib-git
	git clone "https://github.com/mathstuf/xmobar" xmobar-git

solve:
	cp stack.yaml.txt stack.yaml
	stack solve

init:
	stack init
