#!/bin/sh
if [ -e /tmp/win.monitor ]; then
    echo "system_powerdown" | socat - UNIX-CONNECT:/tmp/win.monitor
fi
    
