#!/bin/bash - 
#===============================================================================
#          FILE: t1.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 18.06.2014 08:19
#===============================================================================

set -o nounset                              # Treat unset variables as an error

outfile(){
echo  ${0%%.*}.dat
}

source definitions.sh

value_check                         \
  -F  $VT                           \
  -S jozefoslaw : 52.0 : 21.0 : 110 \
  -D 20120606 : 30@H                \
  -J100 -w n                        \
  -o : $(outfile) :level -H 
