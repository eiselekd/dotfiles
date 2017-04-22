#!/bin/sh

cat > /etc/apt/sources.list <<EOF

deb http://archive.ubuntu.com/ubuntu zesty main universe
deb-src http://archive.ubuntu.com/ubuntu zesty main universe

deb http://security.ubuntu.com/ubuntu zesty-security main universe
deb-src http://security.ubuntu.com/ubuntu zesty-security main universe

deb http://archive.ubuntu.com/ubuntu zesty-updates main universe
deb-src http://archive.ubuntu.com/ubuntu zesty-updates main universe

EOF

ln -s /proc/self/mounts /etc/mtab
apt update

locale-gen en_US.UTF-8

apt install --yes --no-install-recommends linux-image-generic
apt install --yes zfs-initramfs
apt install --yes grub-pc

addgroup --system lpadmin
addgroup --system sambashare

zfs set mountpoint=legacy rpool/var/log
zfs set mountpoint=legacy rpool/var/tmp

cat >> /etc/fstab << EOF
rpool/var/log /var/log zfs defaults 0 0
rpool/var/tmp /var/tmp zfs defaults 0 0
EOF

passwd

cat <<EOF

install grub

EOF

