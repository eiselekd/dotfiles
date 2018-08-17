all:
	@echo "make linux   : start linux"
	@echo "make linux-p : start linux with 05:00.0 as passthrough"
	@echo "make OPTIONS=.. win7  : start uefi (TianaCore) win7"
	@echo "make OPTIONS=.. win10 : start uefi (TianaCore) win10"
	@echo "options: "
	@echo " -n : add networking via br0"
	@echo " -o : use tianacore in ovmf_pkg/"
	@echo " -Q : remove qlx video device"
	@echo " -p : enable passthrough"

linux:
	bash linux_uefi.sh 

linux-p:
	bash linux_uefi.sh -p

remote:
	remote-viewer


win7:
	bash win7_uefi_cpu_host_qemu_vm.sh -o $(OPTIONS)

win10:
	bash win10_uefi_cpu_host_qemu_vm.sh -o $(OPTIONS) 


br0:
	sudo bash br0.sh
