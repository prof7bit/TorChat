#!/bin/sh

trap 'kill -15 `cat tor.pid`' 15

export PATH=$PATH:/usr/sbin
tor -f torrc.temp.txt --PidFile tor.pid &
wait
