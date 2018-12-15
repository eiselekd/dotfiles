#!/bin/sh
if [ -e /tmp/win.monitor ]; then
    echo "system_powerdown" | nc -N 127.0.0.1 55555
    #echo "system_powerdown" | socat - UNIX-CONNECT:/tmp/win.monitor
fi
    
