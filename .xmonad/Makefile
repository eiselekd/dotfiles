all:


pre:
	wget -qO- https://get.haskellstack.org/ | sh

prep:
	git clone "https://github.com/xmonad/xmonad" xmonad-git
	git clone "https://github.com/xmonad/xmonad-contrib" xmonad-contrib-git
	git clone "https://github.com/jaor/xmobar" xmobar-git

solve:
	cp stack.yaml.txt stack.yaml
	stack solve

init:
	stack init
