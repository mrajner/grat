#!/bin/bash - 
#===============================================================================
#          FILE: t3.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 18.06.2014 08:53
#===============================================================================

set -o nounset                              # Treat unset variables as an error

value_check \
  -F ../data/test_data.nc@SP  -V -Sj
  ncdump ../data/test_data.nc
:

