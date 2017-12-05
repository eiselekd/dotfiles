#!/bin/bash

set -euxo pipefail
cd ~/git

export d=/home/eiselekd/git
export n=`getconf _NPROCESSORS_ONLN`

dogit()
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

dospl ()
{
    cd spl
    git reset --hard HEAD
    git pull --rebase
    sh autogen.sh
    ./configure --with-linux=$d/linux --with-linux-obj=$d/linux || exit 1
    { V=1 make     2>&1     | tee log.txt     } || exit 1;
    { V=1 make deb 2>&1     | tee log_deb.txt } || exit 1;
}

dospldeb ()
{
    cd spl
    # install spl for zfs build
    sudo dpkg -i *deb
}

dolinux ()
{
    (cd linux; DEB_BUILD_OPTIONS="debug nostrip noopt" make -j `getconf _NPROCESSORS_ONLN` deb-pkg LOCALVERSION=-custom)
}

installlinux ()
{
    cd ~/git
    sudo dpkg -i linux-*.deb
    (cd linux DEB_BUILD_OPTIONS="debug nostrip noopt" make -j `getconf _NPROCESSORS_ONLN` modules_install LOCALVERSION=-custom)
}

dozfs ()
{
    cd  zfs
    git reset --hard HEAD
    git pull --rebase
    sh autogen.sh
    ./configure --with-spl=$d/spl --with-spl-obj=$d/spl --with-linux=$d/linux --with-linux-obj=$d/linux || exit 1
    { V=1 make 2>&1     | tee log.txt     } || exit 1;
    { V=1 make deb 2>&1 | tee log_deb.txt } || exit 1;
}

dozfsdeb ()
{
    cd zfs
    # install spl for zfs build
    sudo dpkg -i *deb
}

dokernelrearrange ()
{
    cd /lib/modules/<version>
    mkdir kernel/zfs
    cp -r extra/zfs/*  kernel/zfs/
    cp -r extra/spl/*  kernel/zfs/
    depmod -a <version>
}

case "$1" in
    git)
	dogit
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
esac

