#!/bin/sh
bash ./win10_uefi_cpu_host_qemu_vm.sh -M -o -n -u &
sleep 5
remote-viewer spice://127.0.0.1:5900
