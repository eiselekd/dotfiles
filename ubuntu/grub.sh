
# /etc/default/grub
# update-grub
# grub-install /dev/disk/by-id/<id>

# zfs snapshot rpool/ROOT/ubuntu@install
# exit

# mount | grep -v zfs | tac | awk '/\/mnt/ {print $3}' | xargs -i{} umount -lf {}
# zpool export rpool
# reboot

# after /etc/default:
# update-initramfs -c -k all
