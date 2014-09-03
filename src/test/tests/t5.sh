#!/bin/bash - 
#===============================================================================
#          FILE: t5.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 27.06.2014 09:38
#===============================================================================

set -o nounset                              # Treat unset variables as an error

counter=0

. definitions.sh

for exclamation in "" "-!" ; do

  value_check                           \
    ${exclamation}                      \
    -F $VT                              \
    -S g:20 -J 1000, 400 , 200 -o       \
    &> ${0/.sh/.dat}${counter}${suffix}
  let counter++

done

touch ${0/.sh/.dat}${suffix}

