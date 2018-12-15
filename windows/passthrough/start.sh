#!/bin/sh
b=$(dirname `readlink -f $0`)

if cat /proc/cmdline | grep "efifb:off"; then
    cd ${b}
    #-u
    socat TCP-LISTEN:44444,fork,reuseaddr TCP:localhost:55555 &
    bash ${b}/win10_uefi_cpu_host_qemu_vm.sh -M -p -o -n -r -V  &
    echo $! > /tmp/vm-start.pid
else
    (while true; do sleep 100; done) &
    echo $! > /tmp/vm-start.pid
fi
