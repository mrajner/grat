#!/bin/bash - 
#===============================================================================
#          FILE: t14.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 02.10.2014 15:38
#===============================================================================

set -e
set -o nounset 
set -o pipefail

. definitions.sh

{
  grat \
    -S j \
    -F ${EWT-100@EWT}  \
    -M2 -G ../../../dat/merriam_green.dat @GR : 1 : 6 @ m2f 
} | tee ${0/.sh/.dat}${suffix}
