#!/bin/sh

if [ -z ${D} ]; then echo "disk id missing"; exit 1; fi

apt install dosfstools
mkdosfs -F 32 -n EFI ${D}-part2
mkdir /boot/efi
echo PARTUUID=$(blkid -s PARTUUID -o value \
      ${D}-part2) \
      /boot/efi vfat nofail,x-systemd.device-timeout=1 0 1 >> /etc/fstab
mount /boot/efi

apt install --yes grub-efi-amd64-signed shim-signed

cat <<EOF > /etc/systemd/system/zfs-import-bpool.service
[Unit]
DefaultDependencies=no
Before=zfs-import-scan.service
Before=zfs-import-cache.service

[Service]
Type=oneshot
RemainAfterExit=yes
ExecStart=/sbin/zpool import -N -o cachefile=none bpool

[Install]
WantedBy=zfs-import.target
EOF

systemctl enable zfs-import-bpool.service
