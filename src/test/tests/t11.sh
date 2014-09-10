#!/bin/bash - 
#===============================================================================
#          FILE: t11.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 07.09.2014 19:26
#===============================================================================

set -e
set -o nounset 
set -o pipefail

. definitions.sh

{

  for exclamation in "" "-!" ; do

    value_check \
      ${exclamation} \
      -F ${GP} \
      -S 10,j , r \
      -J 1000,100, 66 \
      -V

    value_check  \
      ${exclamation} \
    -F $SP -S out : 400 :200 , out: 99:355


    value_check \
      ${exclamation} \
      -F $SP -D 20120101,2009,2004 -Sj

    value_check \
      ${exclamation} \
      -F non_exisiting_path , another_one -Sb -H

    value_check  \
      ${exclamation} \
      -F $SP ,$GP -S out : 400 :200 , b -J1000, 500

    exit
  done

} 2>${0/.sh/.dat1}${suffix} | tee  ${0/.sh/.dat}${suffix}

