#!/bin/bash - 
#===============================================================================
#          FILE: t13.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 30.09.2014 21:34
#===============================================================================

set -e
set -o nounset 
set -o pipefail

. definitions.sh


{
  grat -F $SP, $LS -M2 -G@GN,@GE -Sj, r -I-1000@DD : 100@AS -H -rt
  grat -F $SP, $LS -M2 -G@GN,@GE -Sj, r -I-1000@DD : 100@AS -H -rt,nc
  grat -F $SP, $LS -M2 -G@GN,@GE -Sj, r -I-1000@DD : 100@AS -H -rnc
} | tee ${0/.sh/.dat}${suffix}

