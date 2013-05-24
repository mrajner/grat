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
for co in n l
do
  value_check                                                     \
    -F /home/mrajner/dat/ncep_reanalysis/pres.sfc.2011.nc@SP:pres \
    -S 2.51/4.99/0.05/2.45:0.091:0.1 -I ${co} @ I                 \
    -V                                                            \
    -o interp${co}1.dat                                           \
    -L interp1.dat@l
done
perl -n -i -e 'print if $. <= 4' interp1.dat

