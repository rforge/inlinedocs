#!/bin/bash
# vim:set ff=unix expandtab ts=2 sw=2:
tmpDir=$2
mkdir -p ${tmpDir}
timer="${tmpDir}/lastrun$1"
touch $timer
echo $3
while [ TRUE ]; do
  num=$(find $3  -maxdepth 0 -newer ${timer} |wc -l)
  if [ $num -gt 0 ] 
    then	
      touch ${timer}
      #./isall.R
      eval "./${1} ${tmpDir}"
    else 
      sleep 1
  fi
  
done
