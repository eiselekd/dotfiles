# X11:
apt update
apt install emacs xmonad git xmobar xinit software-properties-common build-essential chromium-browser
apt install dmenu
add-apt-repository universe
apt update

adduser eiselekd

git config --global user.email "eiselekd@gmail.com"
git config --global user.name "Konrad Eisele"

ln -s ~/git/dotfiles/.xmonad
rm -rf ~/.emacs.d
ln -s ~/git/dotfiles/.emacs.d

cat >> .bashrc <<EOF
if [ -f ~/git/dotfiles/.bashrc_extra ]; then
   . ~/git/dotfiles/.bashrc_extra
fi
EOF
