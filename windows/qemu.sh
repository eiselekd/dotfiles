#!/bin/bash

qemu-system-x86_64 \
-enable-kvm \
-M q35 \
-m 5120 \
-cpu host \
-boot order=dc \
-monitor stdio \
-debugcon file:/dev/stderr -global isa-debugcon.iobase=0x402 \
-smp 4,sockets=1,cores=2,threads=2 \
-bios /usr/lib/qemu/bios.bin \
-device vfio-pci,host=01:00.0,bus=ich9-pcie-port-1,rombar=0 \
-device vfio-pci,host=09:00.0,bus=ich9-pcie-port-3 \
-device vfio-pci,host=0c:00.0,bus=ich9-pcie-port-4 \
-device vmware-svga,bus=ich9-pci-bridge \
-device vfio-pci,host=07:04.0,bus=ich9-pci-bridge \
-drive file=/mnt/data-n0/vm/win10-win10.qcow2,id=hdd,format=raw -device ide-hd,drive=hdd,bus=ide.1 \
-device usb-tablet \
-net none

