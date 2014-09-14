#!/bin/bash - 
#===============================================================================
#          FILE: speed.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 03.09.2014 06:00
#===============================================================================

shopt -s nullglob

for flags in O3  ; do
  cd $GS ; make clean  ; make all FFLAGS=-$flags FC=ifort link ; make all FFLAGS=-$flags FC=ifort link ; cd - ; make clean 

  VERSION=$(grat -v  | sed -n  -e 's/#\s*//g' -e 's/-[^-]*//2' -e 3p | tr "\n" " ")
  OPT=$(grat -v  | sed -n  -e 's/#\s*//g' -e 's/\(.*-O\)\([0-9]\).*/\2/' -e 7p)
  COMPILER=$(grat -v  | grep "compiler:" | sed 's/\(^.*compiler: \)\([^[[:space:]]*\).*/\2/')


  {      \
    time \
    { 
      make 
      echo TOTAL
    }  ; } 2>&1 \
      | awk \
      -v version=$VERSION \
      -v opt=$OPT \
      -v date="$(date)" \
      -v host=$HOSTNAME \
      -v comp=$COMPILER \
      ' 
    @include "/home/mrajner/.local/src/awk/color.awk"
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
      printf "%10s %6s %10s %1i %10s %10s %10s %10s\n",
      host, comp, version, opt, FILE[i] , TIME[i]["real"], TIME[i]["user"] , TIME[i]["sys"]
    }
  }' | tee -a time.dat
done
