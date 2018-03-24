ZFSDIR=-I/usr/include/libspl -I/usr/include/libzfs

all:
	gcc -g $(ZFSDIR) zfs_copy_label.c -o zfs_copy_label.exe -lnvpair -lzfs

prepare:
	export DEB_BUILD_OPTIONS="debug nostrip noopt"; apt-get -b source libzfslinux-dev
	git clone https://github.com/zfsonlinux/zfs zfs-src
	git clone https://github.com/zfsonlinux/spl spl-src

raidz-calc.exe: raidz-calc.c
	gcc -g -o raidz-calc.exe raidz-calc.c  -lm
