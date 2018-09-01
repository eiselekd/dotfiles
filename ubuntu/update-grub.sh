#!/bin/sh

apt install dosfstools
mkdosfs -F 32 -n EFI /dev/disk/by-id/nvme-Samsung_SSD_970_EVO_1TB_S467NF0K701669M-part3
mkdir /boot/efi
echo PARTUUID=$(blkid -s PARTUUID -o value \
      /dev/disk/by-id/nvme-Samsung_SSD_970_EVO_1TB_S467NF0K701669M-part3) \
      /boot/efi vfat nofail,x-systemd.device-timeout=1 0 1 >> /etc/fstab
mount /boot/efi
apt install --yes grub-efi-amd64

