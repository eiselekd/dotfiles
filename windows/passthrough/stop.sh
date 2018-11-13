#!/bin/sh
echo "system_powerdown" | socat - UNIX_CONNECT:/tmp/win.monitor
