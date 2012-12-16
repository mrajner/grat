#!/bin/bash - 
#===============================================================================
#          FILE: interpolation_ilustration.sh
#         USAGE: ./interpolation_ilustration.sh 
#   DESCRIPTION: 
#       OPTIONS: ---
#        AUTHOR: mrajner
#       CREATED: 05.12.2012 10:38:30 CET
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
for co in n b
do
   if [ ${co} = "b" ] ; then 
     interp=2
   else
     interp=1
   fi
  ../bin/value_check -F../data/ncep_reanalysis/pres.sfc.2012.nc:pres \
    -S2.51/4.99/0.05/2.45,0.091,0.1 -I${interp}  -ointerp${co}1.dat -Linterpl1.dat
done
  perl -n -i -e 'print if $. <= 4' interpl1.dat

