#!/bin/bash - 
#===============================================================================
#          FILE: speed.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 03.09.2014 06:00
#===============================================================================

shopt -s nullglob

OUT=${0/.sh/.dat}

touch t3.sh t2.sh

VERSION=$(grat -v  | sed -n  -e 's/#\s*//g' -e 's/-[^-]*//2' -e 3p | tr "\n" " ")
OPT=$(grat -v  | sed -n  -e 's/#\s*//g' -e 's/\(.*-O\)\([0-9]\).*/\2/' -e 7p)

{      \
  time \
  { 
    make t[23].dat*c
    echo TOTAL
  }  ; } 2>&1 \
    | awk '
  {
    if(/^time.**bash/ || /^TOTAL/){
      FILE[++i] = gensub(/(.*bash\s*)(.*\.sh [crs])(.*)/,"\\2","g",$0) 
    }

    if(/^real|^user|^sys/){
      TIME[i][$1]=$2
    }
  }

  END {
  for (i=1;i<=length(FILE);i++){
    print FILE[i] , TIME[i]["real"], TIME[i]["user"] , TIME[i]["sys"]
  }
}'

