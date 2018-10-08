#!/bin/bash - 
#===============================================================================
#          FILE: t7.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 03.09.2014 14:40
#===============================================================================

set -e
set -o nounset 
set -o pipefail

. ./definitions.sh

{
  value_check                                         \
    -o /dev/null

  grat -Sj -F$SP -L @p -M2 -G@GN -o /dev/null -I5@DE: 90@AS :-290 @DD -q

} | tee ${0/.sh/.dat}${suffix}
