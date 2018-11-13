#!/bin/sh

if cat /proc/cmdline | grep "efifb:off"; then
    cd /mnt/data-n0/vms-win/
    bash /mnt/data-n0/vms-win/win10_uefi_cpu_host_qemu_vm.sh -M -p -o -n -u &
    echo $! > /mnt/data-n0/vms-win/start.pid
else
    (while true; do sleep 100; done) &
    echo $! > /mnt/data-n0/vms-win/start.pid
fi
