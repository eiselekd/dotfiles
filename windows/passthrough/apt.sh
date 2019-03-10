#!/bin/bash
b=$(pwd)

passthrough=0
qxl=1
uefi=0
ovmf=1
net=0
nvidiavendor=
usbinput=0
monitor=1
shutdown=0
virtioinput=0
qmp=1
imgdir=/mnt/data-vm/vms-win
while getopts "bpQUonVuMr" opt; do
  case $opt in
      r) shutdown=1;;
      p) passthrough=1 ;;
      Q) qxl=0 ;;
      U) uefi=0 ;;
      u) usbinput=1;;
      n) net=1 ;;
      o) ovmf=1 ;;
      M) monitor=0 ;;
      I) virtioinput=1;;
      V) nvidia=1; nvidiavendor=",hv_vapic,hv_time,hv_vendor_id=whatever" ;;
  esac #hv_relaxed,hv_spinlocks=0x1fff,hv_relaxed,
done

OPTS=""
# Basic CPU settings.
OPTS="$OPTS -cpu host,kvm=off${nvidiavendor}"
OPTS="$OPTS -smp 2,sockets=1,cores=2,threads=1"
# Enable KVM full virtualization support.
OPTS="$OPTS -enable-kvm"
# Assign memory to the vm.
OPTS="$OPTS -m 1000"

# Load our created VM image as a harddrive.
# /home/eiselekd/aptiv-win.vdi
OPTS="$OPTS -hda /mnt/usb0/vm/win10/Snapshots/{7dace848-fc2d-4080-a69f-0d4614ac34d3}.vdi"

# Load our OS setup image e.g. ISO file.
#OPTS="$OPTS -cdrom ${b}/windows_10.iso"
#OPTS="$OPTS -cdrom ${imgdir}/virtio-win-0.1.141.iso"

OPTS="$OPTS -vga cirrus"

#if [ "$qxl" == "1" ]; then
#    # Use the following emulated video device (use none for disabled).
#    OPTS="$OPTS -vga qxl"
#else
#    # Use an emulated video device (use none for disabled).
#    #  -vga none -device qxl
#    OPTS="$OPTS -vga none -device qxl "
#fi
#OPTS="$OPTS -spice port=5900,addr=127.0.0.1,disable-ticketing "

if [ "${net}" == "1" ]; then
    # Improve the network performance by utilizing virtio-net.
    OPTS="$OPTS -device virtio-net,netdev=net0,mac=de:ad:be:ef:33:4a"
    OPTS="$OPTS -netdev tap,id=net0,ifname=vmtap0,script=./qemu-ifup,downscript=./qemu-ifdown"
else
    OPTS="$OPTS -net none "
fi

# USB mouse
# echo "Install virtio-input drivers in start-remote-viewer.shand module virtio_input"

OPTS="$OPTS -usb"
OPTS="$OPTS -device usb-ehci,id=ehci"

# Redirect QEMU's console input and output.
if [ "$monitor" == "1" ]; then
    OPTS="$OPTS -monitor stdio"
else
    #OPTS="$OPTS -monitor unix:/tmp/win.monitor,server,nowait "
    OPTS="$OPTS -monitor tcp:127.0.0.1:55555,server,nowait "
fi

if [ "$qmp" == "1" ]; then
    OPTS="$OPTS -qmp tcp:localhost:4444,server,nowait "
fi

exec sudo qemu-system-x86_64 $OPTS
