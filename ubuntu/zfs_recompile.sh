#!/bin/bash


cd ~/git
#git clone https://github.com/zfsonlinux/spl; (cd spl; git checkout master;)
#git clone https://github.com/zfsonlinux/zfs; (cd zfs; git checkout master;)
(cd spl; git pull --rebase)
(cd zfs; git pull --rebase)

export d=`pwd`
export n=`getconf _NPROCESSORS_ONLN`

#(cd linux; DEB_BUILD_OPTIONS="debug nostrip noopt" make -j `getconf _NPROCESSORS_ONLN` deb-pkg LOCALVERSION=-custom)

(
    cd spl
    git reset --hard HEAD
    git pull --rebase
    sh autogen.sh
    ./configure --with-linux=$d/linux --with-linux-obj=$d/linux || exit 1
    make     || exit 1;
    make deb || exit 1;

    # install spl for zfs build
    sudo dpkg -i *deb
    
)

(
    cd  zfs
    git reset --hard HEAD
    git pull --rebase
    sh autogen.sh
    ./configure --with-spl=$d/spl --with-spl-obj=$d/spl --with-linux=$d/linux --with-linux-obj=$d/linux || exit 1
    make     || exit 1;
    make deb || exit 1;
)
