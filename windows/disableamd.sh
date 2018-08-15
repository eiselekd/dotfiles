d=$1

# http://vfio.blogspot.com/2015/05/vfio-gpu-how-to-series-part-3-host.html
# echo "options vfio-pci ids=1002:68f9,1002:aa68" >> ${d}/etc/modprobe.d/vfio.conf
echo "install vfio-pci /sbin/vfio-pci-override-vga.sh" >> ${d}/etc/modprobe.d/vfio.conf
echo "blacklist radeon"                                >> ${d}/etc/modprobe.d/blacklist.conf

cp vfio-pci-override-vga.sh ${d}/sbin/vfio-pci-override-vga.sh
chmod a+rwx ${d}/sbin/vfio-pci-override-vga.sh
