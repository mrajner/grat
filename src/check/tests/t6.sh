#!/bin/bash - 
#===============================================================================
#          FILE: t6.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 27.06.2014 09:49
#===============================================================================

set -o nounset                              # Treat unset variables as an error

grat                                \
  -F                                \
  ../data/pres.sfc.2012.nc @SP:pres \
  -M1,2                             \
  -G rajner @GN                     \
  -S joze:52:21:100                 \
  -D2012:50@H : 2@H                 \
  -o ${0/.sh/.dat}                  \
  -V ${0/.sh/.dat1} 2>${0/.sh/.dat2}

