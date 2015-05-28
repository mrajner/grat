#!/bin/bash - 
#===============================================================================
#          FILE: t4.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 18.06.2014 09:03
#===============================================================================

set -o nounset                              # Treat unset variables as an error

counter=0

. definitions.sh

for exclamation in "" "-!" ; do

  grat                                  \
    ${exclamation}                      \
    -F $SP                              \
    -G merriam @GN                      \
    -S pl:5.2                           \
    -M2 -Dm                             \
    -o ${0/.sh/.dat}${counter}${suffix} \
    &> ${0/.sh/.dat}$((counter+1))${suffix}
  let counter=counter+2

done
  
touch ${0/.sh/.dat}${suffix}
