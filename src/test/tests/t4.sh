#!/bin/bash - 
#===============================================================================
#          FILE: t4.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 18.06.2014 09:03
#===============================================================================

set -o nounset                              # Treat unset variables as an error

grat \
  -F ../data/pres.sfc.2012.nc @SP : pres \
  -G merriam @GN \
  -S pl -M2 -Dm -o t4.dat

