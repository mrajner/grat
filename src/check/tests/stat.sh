#!/bin/bash - 
#===============================================================================
#          FILE: stat.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 18.06.2014 08:46
#===============================================================================

set -o nounset                              # Treat unset variables as an error
shopt -s nullglob

counter=0
good=0
bad=0

for i in t*.dat ; do
  diff $i ${i/t/r} >/dev/null \
   && { tput setaf 2 ;  echo "$i -- passed" ; tput sgr0 ; let good++; : ; } \
   || { tput setaf 1 ;  echo "$i -- failed" ; tput sgr0 ; let bad++ ; : ; } 

  let counter++
done
echo tests: $good/$counter
[[ $bad -gt 0 ]] && echo failed: $bad || :

