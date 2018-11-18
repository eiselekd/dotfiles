#!/bin/bash

b=$(pwd)

passthrough=0
qxl=1
uefi=1
ovmf=0
net=0
while getopts "bpQUon" opt; do
  case $opt in
      p) passthrough=1 ;;
      Q) qxl=0 ;;
      U) uefi=0 ;;
      n) net=1 ;;
      o) ovmf=1 ;; # use tianacore
  esac
done

cp ${b}/uefi/usr/share/edk2.git/ovmf-x64/OVMF_VARS-pure-efi.fd .
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
    if [ "$ovmf" == "0" ]; then
	OPTS="$OPTS -drive if=pflash,format=raw,readonly,file=${b}/uefi/usr/share/edk2.git/ovmf-x64/OVMF_VARS-pure-efi.fd"
	OPTS="$OPTS -drive if=pflash,format=raw,file=${b}/OVMF_VARS-pure-efi.fd"
    else
	mkdir -p ${b}/hda-contents
	OPTS="$OPTS -pflash ${b}/ovmf_pkg/OVMF.fd"
	OPTS="$OPTS -drive if=pflash,format=raw,file=${b}/OVMF_VARS-pure-efi.fd"
    fi
fi

# Load our created VM image as a harddrive.
OPTS="$OPTS -hda ${b}/win7_uefi_cpu_host_qemu_vm.qcow2"
# Load our OS setup image e.g. ISO file.
#OPTS="$OPTS -cdrom ${b}/windows_7.iso"

OPTS="$OPTS -cdrom ${b}/virtio-win-0.1.141.iso"

if [ "$qxl" == "1" ]; then
    # Use the following emulated video device (use none for disabled).
    OPTS="$OPTS -vga qxl"
else
    # Use an emulated video device (use none for disabled).
    OPTS="$OPTS -vga none -device qxl"
fi

# Redirect QEMU's console input and output.
OPTS="$OPTS -monitor stdio"

if [ "${net}" == "1" ]; then
    # Improve the network performance by utilizing virtio-net.
    OPTS="$OPTS -device virtio-net,netdev=net0,mac=de:ad:be:ef:33:4a"
    OPTS="$OPTS -netdev tap,id=net0,ifname=vmtap0,script=./qemu-ifup,downscript=./qemu-ifdown"
else
    OPTS="$OPTS -net none "
fi

OPTS="$OPTS -spice port=5900,addr=127.0.0.1,disable-ticketing "

sudo qemu-system-x86_64 $OPTS
