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
  grat -F $SP -M2 -G@GN,@GE -Sj -BN -I-10@DD : 100@AS -H -rt -L@p:s -o /dev/null -q
  grat -F $SP -M2 -G@GN,@GE -Sj -BN -I-10@DD : 10@AS :2@DE -H -rt -L@p:s -o /dev/null -q
} | tee ${0/.sh/.dat}${suffix}
