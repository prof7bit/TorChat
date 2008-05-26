#!/bin/sh

trap 'kill -15 `cat tor.pid`' 15

tor -f torrc.txt --PidFile tor.pid &
wait
