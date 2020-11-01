#!/bin/sh
# rebind alt-[left|right|up|down] in virtual console
# make part of rc.local:
# sudo systemctl edit --full rc-local

# sudo systemctl set-default multi-user.target
# sudo systemctl set-default graphical.target

echo "alt keycode 105 = F105" | loadkeys
echo "alt keycode 106 = F106" | loadkeys
echo "alt keycode 103 = F103" | loadkeys
echo "alt keycode 108 = F108" | loadkeys

echo "string F105 = \"\033\033[D\"" | loadkeys
echo "string F106 = \"\033\033[C\"" | loadkeys
echo "string F103 = \"\033\033[A\"" | loadkeys
echo "string F108 = \"\033\033[B\"" | loadkeys
