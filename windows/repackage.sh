#!/bin/bash
b=`pwd`

if [ -z $1 ]; then
    echo "Usage: $0 <execute-script> : where <execute-script> is called with extracted cpio basedir"; exit 1
fi

v=$(uname -r)
while getopts "v:" o; do
    case "${o}" in
        v)
            v=${OPTARG}
            ;;
    esac
done
i=initrd.img-${v}

if [ ! -f dracut/skipcpio/skipcpio ]; then
    git clone https://github.com/dracutdevs/dracut.git
    ( cd dracut/skipcpio; gcc -g -o skipcpio skipcpio.c )
fi

#extract both intel and amd microcode
echo "Using: /boot/initrd.img-${v}"
dracut/skipcpio/skipcpio /boot/${i} > /tmp/a0
dracut/skipcpio/skipcpio /tmp/a0    > /tmp/a1

h=$(( $(stat -c%s /boot/${i}) - $(stat -c%s /tmp/a1) ))
echo "Header size: ${h}"

rm -rf /tmp/b
mkdir -p /tmp/b
cd /tmp/b; zcat /tmp/a1 | cpio --extract

head -c ${h} /boot/${i} > /tmp/h

(cd $b; bash ${b}/${1} /tmp/b)

cpio_owner_root="-R 0:0"
(cd /tmp/b/; find . | cpio --quiet $cpio_owner_root -o -H newc | gzip > /tmp/c )

cat /tmp/h /tmp/c > /tmp/initrd.img-${v}-nographic

ls -la /boot/${i} /tmp/a1 /tmp/h /tmp/c /tmp/initrd.img-${v}-nographic

echo "Use: /tmp/initrd.img-${v}-nographic"
