#!/bin/bash

brctl addbr br0
brctl addif br0 enp0s31f6
ifconfig
ifconfig br0 up
ifconfig
