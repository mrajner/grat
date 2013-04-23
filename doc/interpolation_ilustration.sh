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

## \file
set -o nounset                              # Treat unset variables as an error
for co in n b
do
   if [ ${co} = "b" ] ; then 
     interp=2
   else
     interp=1
   fi
    value_check -F /home/mrajner/dat/ncep_reanalysis/pres.sfc.2011.nc@SP:pres     \
    -S 2.51/4.99/0.05/2.45,0.091,0.1 -I ${interp} \
    -o interp${co}1.dat  -L interpl1.dat :b 
done
  perl -n -i -e 'print if $. <= 4' interpl1.dat

