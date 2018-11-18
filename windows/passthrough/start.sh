#!/bin/sh

if cat /proc/cmdline | grep "efifb:off"; then
    cd /mnt/data-vm/vms-win/
    bash /mnt/data-vm/vms-win/win10_uefi_cpu_host_qemu_vm.sh -M -p -o -n -u -r &
    echo $! > /mnt/data-vm/vms-win/start.pid
else
    (while true; do sleep 100; done) &
    echo $! > /mnt/data-vm/vms-win/start.pid
fi
