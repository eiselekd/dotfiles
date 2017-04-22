zfs list 
zfs snapshot rpool/home/eiselekd@`date +'%Y-%m-%d'`
zfs snapshot rpool/ROOT/ubuntu@`date +'%Y-%m-%d'`
zfs snapshot rpool/home/root@`date +'%Y-%m-%d'`
zfs list -t snapshot
