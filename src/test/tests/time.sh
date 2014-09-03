#!/bin/bash - 
#===============================================================================
#          FILE: speed.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 03.09.2014 06:00
#===============================================================================

shopt -s nullglob
OUT=${0/.sh/.dat}

cat $OUT 2>/dev/null | sed 's/$/infile/'

{ 
  for i in t9.dat* ; do
    egrep "^user" $i -q && 
    {
      line=$(echo -n " $HOSTNAME $i "
      egrep "^(user|real)" $i | tr "\n" " "
      echo)

      grep -q "$line" $OUT 2>/dev/null && : || 
      {
        echo "$line" | tee -a ${OUT}
      }
    } || 
    : 
  done 
} 

shopt -s extglob
TODO

ls !(time)t*dat*
