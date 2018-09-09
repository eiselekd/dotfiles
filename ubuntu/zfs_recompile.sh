#!/bin/bash
set -euxo pipefail

export d=`pwd`
export n=`getconf _NPROCESSORS_ONLN`

function doprep
{
    sudo apt install libattr1-dev
    sudo apt install alien
    #sudo apt install libuuid1-dev
    sudo apt install uuid-dev 
    sudo apt install libblkid-dev libblkid-dev
    sudo apt install libattr1-dev
    sudo apt-get build-dep linux-headers-`uname -r`
}

function dogit
{
    if [ ! -d linux ]; then
	git clone git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git
    fi
    for d in zfs spl; do
	if [ ! -d $d ]; then
	    git clone https://github.com/zfsonlinux/$d; (cd $d; git checkout master;)
	fi
	(cd $d; git reset --hard HEAD)
	(cd $d; git pull --rebase)
    done
}

function prepconfig
{
    cf=config-`uname -r`
    if [ ! -f /boot/${cf} ]; then
	echo "Cannot find kernel config ${cf}"; exit 1;
    fi
    ( cp /boot/${cf} ${d}/linux/.config; cd ${d}/linux; make oldconfig )
}

function dolinux
{
    (cd ${d}/linux; DEB_BUILD_OPTIONS="debug nostrip noopt" make -j `getconf _NPROCESSORS_ONLN` deb-pkg LOCALVERSION=-custom)
}

function installlinux
{
     (cd ${d}/; sudo dpkg -i linux-*.deb )
     (cd ${d}/linux; DEB_BUILD_OPTIONS="debug nostrip noopt" sudo make -j `getconf _NPROCESSORS_ONLN` modules_install LOCALVERSION=-custom)
}

function dospl
{
    cd ${d}/spl
    #git reset --hard HEAD
    #git pull --rebase
    sh autogen.sh
    # todo: ubuntu style lib/exe-dir
    ./configure --with-linux=$d/linux --with-linux-obj=$d/linux || exit 1
    V=1 make ;
    #LC_TIME=C V=1 make deb;
}

function dospldeb
{
    cd ${d}/spl
     # install spl for zfs build
    sudo dpkg -i *deb
}

function dozfs
{
    cd ${d}/zfs
    v=$(cd $d/linux; make kernelversion)-custom
    p="/opt/${v}"

    #git reset --hard HEAD
    #git pull --rebase
    sh autogen.sh
    # todo: ubuntu style lib/exe-dir
    #./configure --with-spl=$d/spl --with-spl-obj=$d/spl --with-linux=$d/linux --with-linux-obj=$d/linux || exit 1

    # build with kernel variant
    ./configure --with-spl=$d/spl --with-spl-obj=$d/spl --with-linux=$d/linux --with-linux-obj=$d/linux \
		--build=x86_64-linux-gnu --prefix=${p}/usr --includedir=\${prefix}/include --mandir=\${prefix}/share/man --infodir=\${prefix}/share/info --sysconfdir=${p}/etc --localstatedir=/var --disable-silent-rules --libdir=\${prefix}/lib/x86_64-linux-gnu --libexecdir=\${prefix}/lib/x86_64-linux-gnu --disable-maintainer-mode --disable-dependency-tracking --bindir=${p}/usr/bin --sbindir=${p}/sbin --libdir=${p}/lib --with-udevdir=${p}/lib/udev --with-systemdunitdir=${p}/lib/systemd/system --with-systemdpresetdir=${p}/lib/systemd/system-preset --with-systemdpresetdir=${p}/lib/systemd/system-preset --with-systemdgeneratordir=${p}/usr/lib/systemd/system-generators --with-config=user --with-systemdmodulesloaddir=${p}//usr/lib/modules-load.d --with-mounthelperdir=${p}/sbin --with-dracutdir=${p}/usr/lib/dracut/modules.d/02zfsexpandknowledge


    
    V=1 make;
    v=1 make install;
    #LC_TIME=C V=1 make deb;
}

function dozfsdeb
{
    cd ${d}/zfs
    # install spl for zfs build
    sudo dpkg -i *deb
}

function dokernelrearrange
{
    v=$(cd $d/linux; make kernelversion)-custom
    cd /lib/modules/${v}
    if [ ! -d /lib/modules/${v}/extra/zfs ]; then
	echo "modules dir /lib/modules/${v}/extra/zfs not found"; exit 1
    fi
    sudo mkdir -p kernel/zfs
    sudo cp -r extra/zfs/*  kernel/zfs/
    sudo cp -r extra/spl/*  kernel/zfs/
    sudo depmod -a ${v}
}

# # for root on zfs
# function doextra 
# {
#     if cat /etc/initramfs-tools/modules | grep zfs ; then
# 	echo "zfs" >> /etc/initramfs-tools/modules
# 	update-initramfs -u
#     fi
# }


if [ -z "$1" ]; then
    echo "$0 [git,linux]"; exit 1
fi

case "$1" in
    prep)
	doprep
	;;
    git)
	dogit
	;;
    prepconfig)
	prepconfig
	;;
    linux)
	dolinux
	;;
    installlinux)
	installlinux
	;;
    spl)
	dospl
	;;
    spldeb)
	dospldeb
	;;
    zfs)
	dozfs
	;;
    zfsdeb)
	dozfsdeb
	;;
    rearrange)
	dokernelrearrange
	;;
    *)
	echo "$1 command unknown"; exit 1
	;;
esac

