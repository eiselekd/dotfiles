#!/bin/sh

echo "$(pwd): archive: $0 -- ${1//\\/\\\\}" >> /cygdrive/c/temp/a.txt
f=$(cygpath -u "${1//\\/\\\\}")
d=$(dirname "$f")
b=$(basename "$f")
echo "$(pwd): $f -- $d -- $b" >> /cygdrive/c/temp/a.txt
( cd "$d"; tar czvf "${b}.tar.gz" "${b}" )



