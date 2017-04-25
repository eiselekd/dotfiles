#!/bin/sh
#set -x

d=`dirname $0`
b=`readlink -f $d`
 
ds=$1
from=$2
to=$3

mp=`zfs get mountpoint $ds | head -2 | tail -1 | awk '{ print $3 }'`

echo "Available snapshots for $ds"
zfs list -t snapshot -r $ds

echo "Generate diff from '${from}'->'${to}'"
zfs diff ${ds}"@"${from} ${ds}"@"${to} > /tmp/_zfs_gen_output.txt

echo "generate report '${from}'->'${to}', mountpoint $ds : '$mp'"
perl ${b}/zfs_diff.pl --from=${mp}/.zfs/snapshot/${from} --to=${mp}/.zfs/snapshot/${to} /tmp/_zfs_gen_output.txt
