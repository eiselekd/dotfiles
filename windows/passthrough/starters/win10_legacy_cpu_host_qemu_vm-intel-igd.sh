#!/bin/bash
b=$(pwd)

passthrough=0
qxl=0
uefi=0
ovmf=0
net=0
nvidia=0
usbinput=0
nvidiavendor=
monitor=1
while getopts "bpQUonVuM" opt; do
  case $opt in
      p) passthrough=1 ;;
      Q) qxl=0 ;;
      U) uefi=0 ;;
      u) usbinput=1;;
      M) monitor=0 ;;
      n) net=1 ;;
      o) ovmf=1 ;;
      V) nvidia=1; nvidiavendor=",hv_vapic,hv_time,hv_vendor_id=whatever" ;;
  esac
done

cp ${b}/uefi/usr/share/edk2.git/ovmf-x64/OVMF_VARS-pure-efi.fd .
OPTS=""
# Basic CPU settings.

OPTS="$OPTS -cpu host,kvm=off${nvidiavendor}"
OPTS="$OPTS -smp 4,sockets=1,cores=4,threads=1"
# Enable KVM full virtualization support.
OPTS="$OPTS -enable-kvm"
# Assign memory to the vm.
OPTS="$OPTS -m 4000"

OPTS="$OPTS -M pc,igd-passthru=on"

# VFIO GPU and GPU sound passthrough.
if [ "$passthrough" == "1" ]; then


    
    if [ "$nvidia" == "1" ]; then
	echo "passthrough nvidia legacy"
	# nvidia_patched.rom romfile=${b}/bioses/nvidia.rom,,x-vga=on
	OPTS="$OPTS -device vfio-pci,host=01:00.0,multifunction=on"
	OPTS="$OPTS -device vfio-pci,host=01:00.1"
    else
	#,multifunction=on,romfile=/mnt/nvidia_efi.rom" ,romfile=${b}/bioses/XFX.HD5450.1024.110612.rom
	#OPTS="$OPTS -device vfio-pci,host=05:00.0,multifunction=on,romfile=${b}/bioses/XFX.HD5450.1024.110612.rom"
	#,romfile=${b}/bioses/XFX.HD5450.1024.110612_1.rom

	#OPTS="$OPTS -vnc :1 "
	#OPTS="$OPTS -machine kernel_irqchip=on" 
	#OPTS="$OPTS -global PIIX4_PM.disable_s3=1 -global PIIX4_PM.disable_s4=1 "

	# for Qemu 2.12 you should add "display=off" option when you create VM without dma-buf
	
	#OPTS="$OPTS -device vfio-pci,sysfsdev=/sys/bus/pci/devices/0000:00:02.0/fd0a9d4e-a969-11e8-a146-ab7f3abe475c,rombar=0 "
	#OPTS="$OPTS -device vfio-pci,host=00:02.0,multifunction=on,x-vga=on"

	OPTS="$OPTS -machine pc -smbios type=2 -nographic " 

	OPTS="$OPTS -device vfio-pci,host=00:02.0,x-igd-opregion=on"
	#OPTS="$OPTS -device vfio-pci,host=00:02.0,bus=pci.0,addr=0x2,x-igd-opregion=on "

	
	#OPTS="$OPTS -vnc 0.0.0.0:1,password=off -vga none "
	OPTS="$OPTS -vga none "
	
	#,romfile=${b}/bioses/intel.bin
	#,romfile=/mnt/data-n0/vms-win/romfile_radeon.bin
	#,romfile=/mnt/nvidia_efi.rom"
	#OPTS="$OPTS -device vfio-pci,host=01:00.1"
    fi
fi

# Supply OVMF (general UEFI bios, needed for EFI boot support with GPT disks).
OPTS="$OPTS -bios ${b}/seabios.bin "

# Load our created VM image as a harddrive.
OPTS="$OPTS -hda ${b}/win10_legacy_cpu_host_qemu_vm.qcow2"
# Load our OS setup image e.g. ISO file.

#OPTS="$OPTS -cdrom ${b}/windows_10.iso"
OPTS="$OPTS -cdrom ${b}/virtio-win-0.1.141.iso"

if [ "$qxl" == "1" ]; then
    # Use the following emulated video device (use none for disabled).
    #OPTS="$OPTS -vga none"
    #OPTS="$OPTS -vga qxl"
    true
else
    # Use an emulated video device (use none for disabled).
    #  -vga none -device qxl
    #OPTS="$OPTS -vga none -device qxl "
    true
fi

#OPTS="$OPTS -spice port=5900,addr=127.0.0.1,disable-ticketing "

# Redirect QEMU's console input and output.
# Redirect QEMU's console input and output.
#if [ "$monitor" == "1" ]; then
#    OPTS="$OPTS -monitor stdio"
#fi
#OPTS="$OPTS -monitor stdio"

if [ "${net}" == "1" ]; then
    # Improve the network performance by utilizing virtio-net.
    OPTS="$OPTS -device virtio-net,netdev=net0,mac=de:ad:be:ef:33:4a"
    OPTS="$OPTS -netdev tap,id=net0,ifname=vmtap0,script=./qemu-ifup,downscript=./qemu-ifdown"
else
    OPTS="$OPTS -net none "
fi

#Bus 003 Device 004: ID 1c4f:0002 SiGma Micro Keyboard TRACER Gamma Ivory
#Bus 003 Device 002: ID 1a40:0201 Terminus Technology Inc. FE 2.1 7-port Hub

# USB mouse

# USB mouse
if [ "$passthrough" == "1" ]; then
    if [ "$usbinput" == "1" ]; then
	OPTS="$OPTS -usb"
	OPTS="$OPTS -device usb-ehci,id=ehci"
	OPTS="$OPTS -device usb-host,bus=usb-bus.0,vendorid=0x17ef,productid=0x6019 "
	OPTS="$OPTS -device usb-host,bus=usb-bus.0,vendorid=0x1c4f,productid=0x0002 "
    else


	OPTS="$OPTS -object input-linux,id=kbd1,evdev=/dev/input/by-path/platform-i8042-serio-0-event-kbd,grab_all=on,repeat=on "
	OPTS="$OPTS -object input-linux,id=mouse1,evdev=/dev/input/by-path/platform-i8042-serio-2-event-mouse,grab_all=on,repeat=on "

    fi
fi


#OPTS="$OPTS -device usb-host,hostbus=3,hostaddr=4"
#OPTS="$OPTS -device usb-host,hostbus=3,hostaddr=2"

#OPTS="$OPTS -device usb-host,bus=xhci.0,vendorid=0x1c4f,productid=0x0002 "
#OPTS="$OPTS -device usb-host,bus=xhci.0,vendorid=0x1a40,productid=0x0201 "

#OPTS="$OPTS -usbdevice host:1c4f:0002"
# USB keyboard
#OPTS="$OPTS -usbdevice host:1a40:0201"

sudo qemu-system-x86_64 $OPTS
