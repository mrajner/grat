#!/bin/bash - 
#===============================================================================
#          FILE: t6.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 27.06.2014 09:49
#===============================================================================

set -o nounset                              # Treat unset variables as an error

counter=0

. definitions.sh 

for exclamation in "" "-!" ; do
  for DE in 0 10 20 30 40 ; do

  grat                \
    ${exclamation}    \
    -H \
    -F $SP            \
    -M1,2             \
    -G rajner @GN     \
    -S joze:52:21:100 \
    -D2012:20@H : 3@H \
    -o:free -V             \
    -I $DE @DE : 2@3D \
    &> ${0/.sh/.dat}${counter}${suffix} 
  let counter++
done

done

touch ${0/.sh/.dat}${suffix}


