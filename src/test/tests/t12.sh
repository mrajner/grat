#!/bin/bash - 
#===============================================================================
#          FILE: t12.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 07.09.2014 20:41
#===============================================================================

set -e
set -o nounset 
set -o pipefail

. definitions.sh

{
  grat \
    -F $SP \
    -M2 -G@GN -Sj, r -I-1000@DD : 100@AS \
    -V \
    -L@p  , @s \
    -o /dev/null \
    -q  \
    -H 
  grat -F $SP -M2 -G@GN -Sj, r -I-1000@DD : 100@AS -V -L@p:s, @s -o /dev/null -q -H 
 } | tee ${0/.sh/.dat}${suffix}

