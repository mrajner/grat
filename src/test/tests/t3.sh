#!/bin/bash - 
#===============================================================================
#          FILE: t3.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 18.06.2014 08:53
#===============================================================================

set -o nounset                              # Treat unset variables as an error

. definitions.sh

# test variable modifiers 

  counter=0
for exclamation in "" "-!" ; do

  value_check                  \
    ${exclamation} \
    -H                         \
    -F                         \
    $SP @scale=100 ,           \
    $T  @invscale=1e8,         \
    $GP @invscale=1e8          \
    \
    -S 10/30/40/90 : 12 : 26   \
    -H -o ${0/.sh/.dat}$counter$suffix: level \
    &> ${0/.sh/.dat}$((counter+1))$suffix 
  let counter=counter+2

  value_check                  \
    ${exclamation} \
    -H                         \
    -F                         \
    $SP @scale=100 ,           \
    $T  @invscale=1e8,         \
    $GP @invscale=1e8          \
    \
    -S 10/30/40/90 : 12 : 26   \
    -H -o ${0/.sh/.dat}$counter$suffix : level -J500,200 -wn
  let counter++
done

touch ${0/.sh/.dat}${suffix}
