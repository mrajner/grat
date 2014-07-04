#!/bin/bash - 
#===============================================================================
#          FILE: t3.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 18.06.2014 08:53
#===============================================================================

set -o nounset                              # Treat unset variables as an error

. definitions.sh c

# test variable modifiers 

counter=0
value_check                \
  -F                       \
  $SP @scale=100 ,         \
  $T  @invscale=1e8,         \
  $GP @invscale=1e8         \
                           \
  -S 10/30/40/90 : 12 : 26 \
  -H -o : level


touch ${0/.sh/.dat}

