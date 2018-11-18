all:
	@echo "make OPTIONS=.. win7[-p]  : start uefi (TianaCore) win7"
	@echo "make OPTIONS=.. win10[-p] : start uefi (TianaCore) win10"
	@echo "options: "
	@echo " -n : add networking via br0"
	@echo " -o : use tianacore in ovmf_pkg/"
	@echo " -Q : remove qlx video device"
	@echo " -p : enable passthrough"

win10-legacy:
	bash win10_legacy_cpu_host_qemu_vm.sh -n $(OPTIONS) 

win10-legacy-p:
	bash win10_legacy_cpu_host_qemu_vm.sh -p -n $(OPTIONS) 

win10-legacy-p-nvidia:
	bash win10_legacy_cpu_host_qemu_vm.sh -p -n -V $(OPTIONS) 


win10-uefi:
	bash win10_uefi_cpu_host_qemu_vm.sh -o $(OPTIONS) 

win10-uefi-p:
	bash $(CURDIR)/start.sh

win10-uefi-p-nvidia:
	bash win10_uefi_cpu_host_qemu_vm.sh -p -o -n -u -V $(OPTIONS) 



br0:
	sudo bash br0.sh

remote:
	remote-viewer

win7:
	bash win7_uefi_cpu_host_qemu_vm.sh -o $(OPTIONS)

win7-p:
	bash win7_uefi_cpu_host_qemu_vm.sh -p -o -Q -n

