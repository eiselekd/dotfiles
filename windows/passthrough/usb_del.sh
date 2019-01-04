#!/bin/sh

echo "device_del ice" | nc -N 127.0.0.1 55555
echo "device_del icb" | nc -N 127.0.0.1 55555
echo "device_del uno" | nc -N 127.0.0.1 55555
