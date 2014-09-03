#!/bin/bash - 
#===============================================================================
#          FILE: speed.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 03.09.2014 06:00
#===============================================================================

# set -o nounset 
# set -o pipefail

for i in t9.dat* ; do

  egrep "^user" $i
  egrep "user" $i
done

