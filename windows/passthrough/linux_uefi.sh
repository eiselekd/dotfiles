#!/bin/bash
b=$(pwd)

passthrough=0
uefi=1
ovmf=0
net=0
while getopts "np" opt; do
  case $opt in
      n) net=1 ;;
      p) passthrough=1 ;;
  esac
done

OPTS=""
# Basic CPU settings.
OPTS="$OPTS -cpu host,kvm=off"
OPTS="$OPTS -smp 4,sockets=1,cores=4,threads=1"
# Enable KVM full virtualization support.
OPTS="$OPTS -enable-kvm"
# Assign memory to the vm.
OPTS="$OPTS -m 4000"
# VFIO GPU and GPU sound passthrough.
if [ "$passthrough" == "1" ]; then
    OPTS="$OPTS -device vfio-pci,host=05:00.0,multifunction=on"
    OPTS="$OPTS -device vfio-pci,host=05:00.1"
fi
# Supply OVMF (general UEFI bios, needed for EFI boot support with GPT disks).
if [ "$uefi" == "1" ]; then    
    if [ "$ovmf" == "1" ]; then    
	OPTS="$OPTS -drive if=pflash,format=raw,readonly,file=/usr/share/edk2.git/ovmf-x64/OVMF_CODE-pure-efi.fd"
	OPTS="$OPTS -drive if=pflash,format=raw,file=$(pwd)/OVMF_VARS-pure-efi.fd"
    else
	OPTS="$OPTS -bios ovmf/OVMF.fd"
    fi
fi
# Load our created VM image as a harddrive.
OPTS="$OPTS -hda ${b}/linux_uefi_ubuntu1804.qcow2"
# Load our OS setup image e.g. ISO file.
OPTS="$OPTS -cdrom ${b}/ubuntu1804.iso"
# Use the following emulated video device (use none for disabled).
if [ "$passthrough" == "1" ]; then
    # 
    OPTS="$OPTS  -vga none -device qxl"
else
    OPTS="$OPTS -vga qxl"
fi
# Redirect QEMU's console input and output.
OPTS="$OPTS -monitor stdio"

OPTS="$OPTS -spice port=5900,addr=127.0.0.1,disable-ticketing "

sudo qemu-system-x86_64 $OPTS
