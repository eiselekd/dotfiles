#!/bin/sh

if cat /proc/cmdline | grep "efifb:off"; then
    cd /mnt/data-vm/vms-win/
    bash /mnt/data-vm/vms-win/win10_uefi_cpu_host_qemu_vm.sh -M -p -o -n -u &
    echo $! > /mnt/data-vm/vms-win/start.pid

    cat <<EOF > /tmp/win-monitor.sh
#!/bin/sh
pid=\$(cat /mnt/data-vm/vms-win/start.pid)
echo "Monitor \${pid}"
# monitor qemu and reboot after exit
while true; do
    if kill -0 \$pid 2>/dev/null; then
	true;
    else
	sleep 10
	echo "Start shutdown"
	shutdown now;
    fi
    echo "Continue..."
    sleep 2;
done
echo "Exit loop"
EOF
    (
	echo "start"
	#chmod a+rwx /tmp/win-monitor.sh
	#nohup /tmp/win-monitor.sh &
    ) > /tmp/win-wait-log.txt &
else
    (while true; do sleep 100; done) &
    echo $! > /mnt/data-vm/vms-win/start.pid
fi
