#!/bin/bash - 
#===============================================================================
#          FILE: t5.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 27.06.2014 09:38
#===============================================================================

set -o nounset                              # Treat unset variables as an error

# grat \
  # -F ../data/air.2012.nc @VT \
  # -S joze:52:21:100


value_check \
  -F ../data/air.2012.nc @VT : air \
  -S g:20 -J 1000, 400 , 200 -o ${0/.sh/.dat} > ${0/.sh/.dat1}

