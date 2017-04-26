#!/bin/sh
# define ${D} before call

if [ -z ${D} ]; then echo "disk id missing"; exit 1; fi

hname=homebox
ifname=enp0s31f6
echo "############### ifname:  ${ifname} : change?  ###############"
echo "############### hostname:${hname}  : change?  ###############"

echo "##### need to partition with sgdisk ######"
# sgdisk -Z -n9:-8M:0 -t9:bf07 -c9:Reserved -n2:-8M:0 -t2:ef02 -c2:GRUB  -n3:-512M:0 -t3:ef00 -c3:UEFI -n1:0:0 -t1:bf01 -c1:ZFS <dev>
# sgdisk -Z -n9:-8M:0 -t9:bf07 -c9:Reserved -n2:-8M:0 -t2:ef02 -c2:GRUB -n1:0:0 -t1:bf01 -c1:ZFS /dev/disk/by-id/nvme-eui.0000000001000000e4d25c4ddd934d01

apt-add-repository universe
apt update
apt install --yes debootstrap gdisk zfs-initramfs git emacs

rm -rf /mnt/*

zpool create -f -o ashift=12 \
      -O atime=off -O canmount=off -O compression=lz4 -O normalization=formD \
      -O mountpoint=/ -R /mnt \
      rpool ${D}-part1


zfs create -o canmount=off -o mountpoint=none rpool/ROOT
zfs create -o canmount=noauto -o mountpoint=/ rpool/ROOT/ubuntu
zfs mount rpool/ROOT/ubuntu


zfs create                 -o setuid=off              rpool/home
zfs create -o mountpoint=/root                        rpool/home/root
zfs create -o canmount=off -o setuid=off  -o exec=off rpool/var
zfs create -o com.sun:auto-snapshot=false             rpool/var/cache
zfs create                                            rpool/var/log
zfs create                                            rpool/var/spool
zfs create -o com.sun:auto-snapshot=false -o exec=on  rpool/var/tmp

zfs create -o mountpoint=/tmp                         rpool/tmp

#If you use /srv on this system:
zfs create                                            rpool/srv

#If this system will have games installed:
zfs create                                            rpool/var/games

# If this system will store local email in /var/mail:
zfs create                                            rpool/var/mail

#If this system will use NFS (locking):
zfs create -o com.sun:auto-snapshot=false \
             -o mountpoint=/var/lib/nfs                 rpool/var/nfs


chmod 1777 /mnt/var/tmp
debootstrap zesty /mnt
zfs set devices=off rpool


echo ${hname}                    > /mnt/etc/hostname
echo "127.0.1.1       ${hname}" >> /mnt/etc/hosts


cat > /mnt/etc/network/interfaces.d/${ifname} <<EOF
auto ${ifname}
iface ${ifname} inet dhcp
EOF

cat >> /etc/fstab << EOF
rpool/var/log /var/log zfs defaults 0 0
rpool/var/tmp /var/tmp zfs defaults 0 0
EOF

mount --rbind /dev  /mnt/dev
mount --rbind /proc /mnt/proc
mount --rbind /sys  /mnt/sys

mkdir -p /mnt/tmp/
cp update.sh /mnt/tmp/update.sh
echo "execute /tmp/update.sh in choot"

chroot /mnt /bin/bash --login
