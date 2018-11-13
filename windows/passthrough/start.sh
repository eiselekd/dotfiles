#!/bin/sh

if cat /proc/cmdline | grep "efifb:off"; then
    cd /mnt/data-n0/vms-win/
    bash /mnt/data-n0/vms-win/win10_uefi_cpu_host_qemu_vm.sh -M -p -o -n -u &
    echo $! > /mnt/data-n0/vms-win/start.pid
    (
	# monitor qemu and reboot after exit
	while true; do
	    if kill -0 $(cat /mnt/data-n0/vms-win/start.pid) 2>/dev/null; then
		true;
	    else
		reboot;
	    fi
	    sleep 1;
	done
    ) &
else
    (while true; do sleep 100; done) &
    echo $! > /mnt/data-n0/vms-win/start.pid
fi
