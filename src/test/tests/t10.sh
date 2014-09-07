#!/bin/bash - 
#===============================================================================
#          FILE: t10.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 06.09.2014 23:30
#===============================================================================

set -e
set -o nounset 
set -o pipefail


. definitions.sh
{
  value_check -Sg:75,j,b -F${SP} -P ../../../polygon/baltyk.poly -V -H
  grat -Sg:75,j,b -F${SP} -P ../../../polygon/baltyk.poly -V -H
  polygon_check -Sj,b,o,:57:22 -P ../../../polygon/baltyk.poly -V -H
} &> ${0/.sh/.dat}${suffix}

