#!/bin/sh

echo "$(pwd): archive: $0 -- ${1//\\/\\\\} ${2}" >> /cygdrive/c/temp/a.txt
f=$(cygpath -u "${1//\\/\\\\}")
d=$(dirname "$f")
b=$(basename "$f")
echo "$(pwd): $f -- $d -- $b" >> /cygdrive/c/temp/a.txt

case ${2} in
    gz)  ( cd "$d"; tar -cvzf "${b}.tar.${2}" "${b}" );;
    bz2) ( cd "$d"; tar -cvjf "${b}.tar.${2}" "${b}" );;
    *)   ( cd "$d"; tar -cvf "${b}.tar" "${b}" );;
esac


