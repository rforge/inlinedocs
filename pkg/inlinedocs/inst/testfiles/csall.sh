#!/bin/bash
# vim:set ff=unix expandtab ts=2 sw=2:
timer=lastrun$1
touch $timer
while [ TRUE ]; do
  num=$(find ../../R/*.R  -maxdepth 0 -newer ${timer} |wc -l)
  if [ $num -gt 0 ] 
    then	
      touch ${timer}
      ./sall.r
    else 
      sleep 1
  fi
done
