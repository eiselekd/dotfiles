#!/bin/bash

b=$(pwd)

passthrough=0
qxl=1
uefi=1
ovmf=1
net=0
nvidia=0
nvidiavendor=
usbinput=0
monitor=1
shutdown=0
virtioinput=0
kbd=0
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
      K) kbd=1;;
      V) nvidia=1; nvidiavendor=",hv_vapic,hv_time,hv_vendor_id=whatever" ;;
  esac #hv_relaxed,hv_spinlocks=0x1fff,hv_relaxed,
done

cp ${b}/uefi/usr/share/edk2.git/ovmf-x64/OVMF_VARS-pure-efi.fd .
OPTS=""
# Basic CPU settings.
OPTS="$OPTS -cpu host,kvm=off${nvidiavendor}"
OPTS="$OPTS -smp 8,sockets=1,cores=8,threads=1"
# Enable KVM full virtualization support.
OPTS="$OPTS -enable-kvm"
# Assign memory to the vm.
OPTS="$OPTS -m 4000"

# VFIO GPU and GPU sound passthrough.
if [ "$passthrough" == "1" ]; then
    if [ "$nvidia" == "1" ]; then
	echo "passthrough nvidia legacy"
	OPTS="$OPTS -device vfio-pci,host=01:00.0,multifunction=on,romfile=${b}/bioses/nvidia_patched.rom,x-vga=on"
	OPTS="$OPTS -device vfio-pci,host=01:00.1"
    else
	#,multifunction=on,romfile=/mnt/nvidia_efi.rom" ,romfile=${b}/bioses/XFX.HD5450.1024.110612.rom
	#OPTS="$OPTS -device vfio-pci,host=05:00.0,multifunction=on,romfile=${b}/bioses/XFX.HD5450.1024.110612.rom"
	#,romfile=${b}/bioses/XFX.HD5450.1024.110612_1.rom
	OPTS="$OPTS -device vfio-pci,host=01:00.0,multifunction=on"
	#,romfile=/mnt/data-n0/vms-win/romfile_radeon.bin
	#,romfile=/mnt/nvidia_efi.rom"
	OPTS="$OPTS -device vfio-pci,host=01:00.1"
    fi
fi

# Supply OVMF (general UEFI bios, needed for EFI boot support with GPT disks).
if [ "$uefi" == "1" ]; then
    if [ "$ovmf" == "0" ]; then
	OPTS="$OPTS -drive if=pflash,format=raw,readonly,file=${b}/uefi/usr/share/edk2.git/ovmf-x64/OVMF_VARS-pure-efi.fd"
	OPTS="$OPTS -drive if=pflash,format=raw,file=${b}/OVMF_VARS-pure-efi.fd"
    else
	mkdir -p ${b}/hda-contents
	#OPTS="$OPTS -pflash ${b}/ovmf_pkg/OVMF.fd"
	OPTS="$OPTS -drive if=pflash,format=raw,readonly,file=${b}/ovmf_pkg/OVMF.fd"
	OPTS="$OPTS -drive if=pflash,format=raw,file=${b}/OVMF_VARS-pure-efi.fd"
    fi
fi

# Load our created VM image as a harddrive.
OPTS="$OPTS -hda ${imgdir}/win10_uefi_cpu_host_qemu_vm.qcow2"
OPTS="$OPTS -hdb ${imgdir}/win10_data.qcow2"
# Load our OS setup image e.g. ISO file.

#OPTS="$OPTS -cdrom ${b}/windows_10.iso"
OPTS="$OPTS -cdrom ${imgdir}/virtio-win-0.1.141.iso"

if [ "$qxl" == "1" ]; then
    # Use the following emulated video device (use none for disabled).
    OPTS="$OPTS -vga qxl"
else
    # Use an emulated video device (use none for disabled).
    #  -vga none -device qxl
    OPTS="$OPTS -vga none -device qxl "
fi

# use remote-viewer prog to connect with spice://localhost:5900
OPTS="$OPTS -spice port=5900,addr=127.0.0.1,disable-ticketing "

if [ "${net}" == "1" ]; then
    # Improve the network performance by utilizing virtio-net.
    OPTS="$OPTS -device virtio-net,netdev=net0,mac=de:ad:be:ef:33:4a"
    OPTS="$OPTS -netdev tap,id=net0,ifname=vmtap0,script=./qemu-ifup,downscript=./qemu-ifdown"
else
    OPTS="$OPTS -net none "
fi

#Bus 003 Device 004: ID 1c4f:0002 SiGma Micro Keyboard TRACER Gamma Ivory
#Bus 003 Device 002: ID 1a40:0201 Terminus Technology Inc. FE 2.1 7-port Hub

# https://passthroughpo.st/using-evdev-passthrough-seamless-vm-input/

# USB mouse
echo "Install virtio-input drivers in start-remote-viewer.sh and module virtio_input"

OPTS="$OPTS -usb"
OPTS="$OPTS -device usb-ehci,id=ehci"

if [ "$passthrough" == "1" ]; then
    if [ "$usbinput" == "1" ]; then
	# plexgear keyboard: Holtek Semiconductor, Inc. Keyboard LKS02
	OPTS="$OPTS -device usb-host,bus=usb-bus.0,vendorid=0x04d9,productid=0x1702 "
	# llexgear mouse: Pixart Imaging, Inc. Optical Mouse
	OPTS="$OPTS -device usb-host,bus=usb-bus.0,vendorid=0x093a,productid=0x2521 "

	#OPTS="$OPTS -device usb-host,bus=usb-bus.0,vendorid=0x17ef,productid=0x6019 "
	#OPTS="$OPTS -device usb-host,bus=usb-bus.0,vendorid=0x1c4f,productid=0x0002 "
    else
	#OPTS="$OPTS -device virtio-keyboard-pci,id=kbd0,serial=virtio-keyboard"

	OPTS="$OPTS -device virtio-input-host-pci,evdev=/dev/input/by-path/platform-i8042-serio-0-event-kbd"
	OPTS="$OPTS -device virtio-input-host-pci,evdev=/dev/input/by-path/platform-i8042-serio-1-event-mouse"

	#OPTS="$OPTS -object input-linux,id=kbd1,evdev=/dev/input/by-path/platform-i8042-serio-0-event-kbd,grab_all=on,repeat=on "
	#OPTS="$OPTS -object input-linux,id=mouse1,evdev=/dev/input/by-path/platform-i8042-serio-1-event-mouse,grab_all=on,repeat=on "
    fi

    #OPTS="$OPTS -device virtio-keyboard-pci -device virtio-mouse-pci"
    #OPTS="$OPTS -device virtio-keyboard-pci,id=input0,bus=pci.0,addr=0x8 -device usb-mouse,id=input1,bus=usb.0,port=1"

    #OPTS="$OPTS -usb "
    #OPTS="$OPTS -device usb-ehci,id=ehci"
    #OPTS="$OPTS -device virtio-keyboard-pci,id=input0,bus=pci.0,addr=0x8"
    #OPTS="$OPTS -device usb-mouse,id=input1,bus=ehci.0,port=1"

else

    
    if [ "$kbd" == "1" ]; then
	OPTS="$OPTS -object input-linux,id=kbd1,evdev=/dev/input/by-path/platform-i8042-serio-0-event-kbd,grab_all=on,repeat=on "
	OPTS="$OPTS -object input-linux,id=mouse1,evdev=/dev/input/by-path/platform-i8042-serio-1-event-mouse,grab_all=on,repeat=on "
    fi
	
    #OPTS="$OPTS -usb"
    #OPTS="$OPTS -device usb-ehci,id=ehci"
    ## plexgear keyboard: Holtek Semiconductor, Inc. Keyboard LKS02
    #OPTS="$OPTS -device usb-host,bus=usb-bus.0,vendorid=0x04d9,productid=0x1702 "
    ## llexgear mouse: Pixart Imaging, Inc. Optical Mouse
    #OPTS="$OPTS -device usb-host,bus=usb-bus.0,vendorid=0x093a,productid=0x2521 "

    #OPTS="$OPTS -usb "
    #OPTS="$OPTS -device usb-ehci,id=ehci"
    #OPTS="$OPTS -device virtio-keyboard-pci,id=input0,bus=pci.0,addr=0x8"
    #OPTS="$OPTS -device usb-mouse,id=input1,bus=ehci.0,port=1"
    #OPTS="$OPTS -device virtio-keyboard-pci -device virtio-mouse-pci"
fi
OPTS="$OPTS -k de "

# Redirect QEMU's console input and output.
if [ "$monitor" == "1" ]; then
    OPTS="$OPTS -monitor stdio"
else
    #OPTS="$OPTS -monitor unix:/tmp/win.monitor,server,nowait "
    OPTS="$OPTS -monitor tcp:127.0.0.1:55555,server,nowait "
fi




#if [ "$passthrough" == "1" ]; then
#    OPTS="$OPTS -usb"
#    OPTS="$OPTS -device usb-ehci,id=ehci"
#    OPTS="$OPTS -device usb-host,bus=usb-bus.0,vendorid=0x17ef,productid=0x6019 "
#    OPTS="$OPTS -device usb-host,bus=usb-bus.0,vendorid=0x1c4f,productid=0x0002 "
#fi

#OPTS="$OPTS -device usb-host,hostbus=3,hostaddr=4"
#OPTS="$OPTS -device usb-host,hostbus=3,hostaddr=2"

#OPTS="$OPTS -device usb-host,bus=xhci.0,vendorid=0x1c4f,productid=0x0002 "
#OPTS="$OPTS -device usb-host,bus=xhci.0,vendorid=0x1a40,productid=0x0201 "

#OPTS="$OPTS -usbdevice host:1c4f:0002"
# USB keyboard
#OPTS="$OPTS -usbdevice host:1a40:0201"

if [ "$shutdown" == "0" ]; then
    exec sudo qemu-system-x86_64 $OPTS
else
    ( sudo qemu-system-x86_64 $OPTS ) 2>&1 | tee  /mnt/data-n0/qemu.log
    sudo sync
    sleep 1
    sudo shutdown now
fi
