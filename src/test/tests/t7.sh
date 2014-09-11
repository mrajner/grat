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
    -S j                                              \
    -o /dev/null                                      \
    -L/dev/null@p, /tmp/f1@s , /tmp/f2@u , /tmp/f3@c, \
    /dev/null@p, /tmp/f1@s , /tmp/f2@u , /tmp/fff@c   \
    -V                                                \
    | sed s/.*%.*\)//

  grat -Sj -F$SP -L @p -M2 -G@GN -o /dev/null -I5@DE: 90@AS :-290 @DD

} > ${0/.sh/.dat}${suffix}
