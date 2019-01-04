#!/bin/sh

echo "device_add usb-host,id=ice,vendorid=0x03eb,productid=0x2141" | nc -N 127.0.0.1 55555
echo "device_add usb-host,id=icb,vendorid=0x03eb,productid=0x2142" | nc -N 127.0.0.1 55555
echo "device_add usb-host,id=uno,vendorid=0x2341,productid=0x0043" | nc -N 127.0.0.1 55555
